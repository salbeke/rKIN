#' Estimate Bivariate Normal Ellipse Isotope Niche
#'
#' Calculates the Bivariate Normal Ellipse Polygon for isotopic values at multiple confidence levels. Returns a list of
#' SpatialPolygonsDataFrame, each list item representing the grouping variable (i.e. species).
#'
#' @param data data.frame object containing columns of isotopic values and grouping variables
#' @param x character giving the column name of the x coordinates
#' @param y character giving the column name of the y coordinates
#' @param group character giving the column name of the grouping variable (i.e. species)
#' @param levels Numeric vector of desired percent levels (e.g. c(10, 50, 90). Should not be less than 1 or greater than 100)
#' @param smallSamp logical value indicating whether to override minimum number of samples. Currently 10 samples are required.
#' @return A list of SpatialPolygonsDataFrame, each list item representing the grouping variable.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @examples
#' library(rKIN)
#' data("rodents")
#' #estimate niche overlap between 2 species using bivariate ellipse
#' test.elp<- estEllipse(data=rodents, x="Ave_C", y="Ave_N", group="Species",
#'                      levels=c(50, 75, 95))
#' #determine polygon overlap for all polygons
#' plotKIN(test.elp, scaler=2, title="Ellipse Estimates", xlab="Ave_C", ylab="Ave_N")

estEllipse_sf <- function(data, x, y, group, levels = c(50, 75, 95), smallSamp = FALSE){
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

  # Loop through each unique value of the group column
  grp<- unique(as.character(data[,group]))
  # create the output object for SpatialPolygonsDataFrame(s)
  #spdf.list<- list()
  #sfdf.list <- list()
  # create the output object for SpatialPointsDataFrame(s)
  #spts.list<- list()
  #sfpts.list <- list()

  sf.tmp <- createSPDF_sf()
  for(g in 1:length(grp)){
    df.g<- data[data[,group]==grp[g] , ]
    # Test for the number of samples. If too small, kick an error
    if(nrow(df.g) < 10 & smallSamp == FALSE)
      stop(paste("It appears that group ", grp[g], " has fewer than 10 samples. Please remove group ", grp[g], " from the data.frame."))
    if(nrow(df.g) < 3 & smallSamp == TRUE)
      stop(paste("It appears that group ", grp[g], " has fewer than 3 samples. Please remove group ", grp[g], " from the data.frame."))
    # calculate the centroid of the points to calculate confidence intervals
    cent <- apply(df.g[, c(x, y)], 2, mean)
    # calculate the covariance
    sigma<- stats::cov(cbind(df.g[ , x], df.g[ , y]))

    # create the spatial points data.frame
    # populate the points into the spdf
    # spts.tmp<- sp::SpatialPointsDataFrame(coords = df.g[ , c(x, y)],
    #                                       data = data.frame(Method = rep("Ellipse", nrow(df.g)),
    #                                                         Group = rep(grp[g], nrow(df.g)),
    #                                                         x = df.g[, x], y = df.g[, y]))

    df.tmp <- data.frame(Method = rep("Ellipse", nrow(df.g)),
                         Group = rep(grp[g], nrow(df.g)),
                         x = df.g[, x], y = df.g[, y])

    if (!exists("sfpts.tmp")) {
      sfpts.tmp <- sf::st_as_sf(df.tmp, coords = c("x", "y"), remove = FALSE)
      names(sfpts.tmp)[3:4] <- c(x, y)
    }
    else {
      temp <- sf::st_as_sf(df.tmp, coords = c("x", "y"), remove = FALSE)
      names(temp)[3:4] <- c(x, y)
      sfpts.tmp <- rbind(sfpts.tmp, temp)
    }
    # set column names to the input values
    #names(spts.tmp)[3:4]<- c(x, y)
    # loop through each level
    #sp.tmp<- createSPDF()
    #sf.tmp <- createSPDF_sf()
    for(lev in 1:length(levels)){
      #//////////////////////////////////////
      # Below code directly borrowed from SIBER package
      # calculate the circle radius
      radius<- sqrt(stats::qchisq(levels[lev] / 100, df = 2))
      #e<- eigen(sigma/nrow(df.g))
      e<- eigen(sigma)
      SigSqrt = e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
      cc <- genCircle(n = 100, radius)
      back.trans <- function(bt) {
        return(SigSqrt %*% bt + cent)
      }
      df.xy <- t(apply(cc, 1, back.trans))

      df.xy <- as.data.frame(df.xy)
      names(df.xy) <- c(x, y)
      print(class(df.xy))
      # END SIBER Borrowed portion
      #/////////////////////////////////////
      # create a single spatial polygon
      # rstdy<- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(df.xy[ , ]))), ID = lev)))
      # rstdy<- sp::SpatialPolygonsDataFrame(rstdy, data = data.frame(Method = "Ellipse", Group = grp[g], ConfInt = levels[lev], ShapeArea = rstdy@polygons[[1]]@area), match.ID = FALSE)
      # sp.tmp<- sp::rbind.SpatialPolygonsDataFrame(sp.tmp, rstdy)

      sfStdy <- sf::st_as_sf(df.xy, coords = c(x, y)) |>
        sf::st_combine() |>
        sf::st_cast("POLYGON")

      sfStdy <- sf::st_as_sf(cbind(data.frame(Method = "Ellipse", Group = grp[g], ConfInt = levels[lev], ShapeArea = NA_real_), sfStdy))
      sfStdy$ShapeArea <- sf::st_area(sfStdy$geometry)
      sf.tmp <- rbind(sf.tmp, sfStdy)

    }# close levels
    # add the group polygon to the list of outputs
    #spdf.list<- c(spdf.list, sp.tmp)
    # add the group points to the list of outputs
    #spts.list<- c(spts.list, spts.tmp)
  }# close group loop
  # describe the polygons
  #names(spdf.list)<- grp
  #class(spdf.list)<- "estObj"
  # describe the points
  #names(spts.list)<- grp
  #class(spts.list)<- "estInput"
  # combine the polygons and points
  #sea<- list(estInput = spts.list, estObj = spdf.list)
  sea<- list(estInput = sfpts.tmp, estObj = sf.tmp)
  attr(sea, "package")<- "rKIN"
  return(sea)
}# close function
