data=rodents
x="Ave_C"
y="Ave_N"
group="Species"
levels= c(50, 75, 95)
smallSamp = FALSE

estMCP_sf <- function(data, x, y, group, levels= c(50, 75, 95), smallSamp = FALSE) {
  # need to perform some class testing first before running any below code
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame!")
  if(!inherits(x, "character"))
    stop("x must be a character giving the x coordinate column name!")
  if(x %in% names(data)==FALSE)
    stop("The value of x does not appear to be a valid column name!")
  if(!inherits(data[, x], "numeric"))
    stop("data in column x is not numeric!")
  if(!inherits(y, "character"))
    stop("y must be a character giving the y coordinate column name!")
  if(y %in% names(data)==FALSE)
    stop("The value of y does not appear to be a valid column name!")
  if(!inherits(data[, y], "numeric"))
    stop("data in column y is not numeric!")
  if(!inherits(group, "character"))
    stop("group must be a character giving the grouping variable column name!")
  if(group %in% names(data)==FALSE)
    stop("The value of group does not appear to be a valid column name!")
  if(!inherits(levels, "numeric"))
    stop("levels must be a numeric vector with values ranging between 1 and 100!")
  if(!all(levels > 0 | levels <= 100))
    stop("levels must be a numeric vector with values ranging between 1 and 100!")

  grp<- unique(as.character(data[,group]))
  # create the output object for SpatialPolygonsDataFrame(s)
  sfdf.list<- list()
  # create the output object for SpatialPointsDataFrame(s)
  sfts.list<- list()

  sf.tmp <- createSPDF_sf()

  for(g in 1:length(grp)){
    df.g<- data[data[,group]==grp[g] , ]
    # Test for the number of samples. If too small, kick an error
    if(nrow(df.g) < 10 & smallSamp == FALSE)
      stop(paste("It appears that group ", grp[g], " has fewer than 10 samples. Please remove group ", grp[g], " from the data.frame."))
    if(nrow(df.g) < 3 & smallSamp == TRUE)
      stop(paste("It appears that group ", grp[g], " has fewer than 3 samples. Please remove group ", grp[g], " from the data.frame."))
    # calculate the centroid of the points to estimate distnace confidence intervals
    cent <- apply(df.g[, c(x, y)], 2, mean)
    # function to calculate Euclidean distance
    euc.dist <- function(xy1, xy2) {
      d <- sqrt(((xy1[1] - xy2[1])^2) + ((xy1[2] - xy2[2])^2))
      return(d)
    }
    # create the spatial points data.frame
    # populate the points into the spdf

    # create data frame with coords first
    # then turn into sf object with st_as_sf and remove = FALSE

    df.tmp <- data.frame(Method = rep("MCP", nrow(df.g)),
                         Group = rep(grp[g], nrow(df.g)),
                         x = df.g[, x], y = df.g[, y])

    if (!exists("sfts.tmp")) {
      sfts.tmp <- sf::st_as_sf(df.tmp, coords = c("x", "y"), remove = FALSE)
      names(sfts.tmp)[3:4] <- c(x, y)
    }
    else {
      temp <- sf::st_as_sf(df.tmp, coords = c("x", "y"), remove = FALSE)
      names(temp)[3:4] <- c(x, y)
      sfts.tmp <- rbind(sfts.tmp, temp)

    }

    # set column names to the input values

    # measure distance of each observation to centroid and append to data.frame
    df.g$Dist <- apply(df.g[ , c(x, y)], 1, FUN = function(p){euc.dist(p, cent)})
    # loop through each level
    #sf.tmp <- createSPDF_sf()
    for(lev in 1:length(levels)){
      # filter rows which meet MCP level threshold distance from centroid
      df.xy<- df.g[which(df.g$Dist <= stats::quantile(df.g$Dist, levels[lev] / 100)), c(x, y)]
      # get rows creeating MCP
      df.xy<- df.xy[grDevices::chull(x = df.xy[ , 1], y = df.xy[ , 2]), ]
      # append first row to end to complete the polygon
      df.xy<- rbind(df.xy, df.xy[1, ])
      print(class(df.xy))
      # create a single spatial polygon
      sfStdy <- sf::st_as_sf(df.xy, coords = c(x, y)) |>
        sf::st_combine() |>
        sf::st_cast("POLYGON")

      sfStdy <- sf::st_as_sf(cbind(data.frame(Method = "MCP", Group = grp[g], ConfInt = levels[lev], ShapeArea = NA_real_), sfStdy))
      sfStdy$ShapeArea <- sf::st_area(sfStdy$geometry)
      sf.tmp <- rbind(sf.tmp, sfStdy)

    } # end levels loop
    # add the group polygon to the list of outputs
    #sfdf.list<- c(sfdf.list, sf.tmp)
    # add the group points to the list of outputs
    #sfts.list<- c(sfts.list, sfts.tmp)
  }# close group loop

  # describe the polygons
  #names(sfdf.list)<- grp
  #class(sfdf.list)<- "estObj"
  # describe the points
  #names(sfts.list)<- grp
  #class(sfts.list)<- "estInput"
  # combine the polygons and points
  #mcp<- list(estInput = sfts.list, estObj = sfdf.list)
  mcp<- list(estInput = sfts.tmp, estObj = sf.tmp)
  attr(mcp, "package")<- "rKIN"
  return(mcp)
}

