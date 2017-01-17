#' Estimate Minimum Convex Polygon (MCP) Isotope Niche
#' 
#' Calculates the Minimum Convex Polygon for isotopic values at multiple confidence levels. Returns a list of 
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
#' #estimate niche overlap between 2 species using minimum convex polygons
#' test.mcp<- estMCP(data=rodents, x="Ave_C", y="Ave_N", group="Species", 
#'                    levels=c(50, 75, 95))
#' #determine polygon overlap for all polygons
#' plotKIN(test.mcp, scaler=2, title="Minimum Convex Hull Estimates", xlab="Ave_C", ylab="Ave_N")

estMCP <- function(data, x, y, group, levels= c(50, 75, 95), smallSamp = FALSE){
  # need to perform some class testing first before running any below code
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame!")
  if(!inherits(x, "character"))
    stop("x must be a character giving the x coordinate column name!")
  if(!inherits(y, "character"))
    stop("y must be a character giving the y coordinate column name!")
  if(!inherits(group, "character"))
    stop("group must be a character giving the grouping variable column name!")
  if(!inherits(levels, "numeric"))
    stop("levels must be a numeric vector with values ranging between 1 and 100!")
  if(!all(levels > 0 | levels <= 100))
    stop("levels must be a numeric vector with values ranging between 1 and 100!")
  
  # Loop through each unique value of the group column
  grp<- unique(as.character(data[,group]))
  # create the output object for SpatialPolygonsDataFrame(s)
  spdf.list<- list()
  # create the output object for SpatialPointsDataFrame(s)
  spts.list<- list()
  for(g in 1:length(grp)){
    df.g<- data[data[,group]==grp[g] , ]
    # Test for the number of samples. If too small, kick an error
    if(nrow(df.g) < 10 & smallSamp == FALSE)
      stop(paste("It appears that group ", grp[g], " has fewer than 10 samples. Please remove group ", grp[g], " from the data.frame."))
    # calculate the centroid of the points to estimate distnace confidence intervals
    cent <- apply(df.g[, c(x, y)], 2, mean)
    # function to calculate Euclidean distance
    euc.dist <- function(xy1, xy2) {
      d <- sqrt(((xy1[1] - xy2[1])^2) + ((xy1[2] - xy2[2])^2))
      return(d)
    }
    # create the spatial points data.frame
    # populate the points into the spdf
    spts.tmp<- sp::SpatialPointsDataFrame(coords = df.g[ , c(x, y)],
                                          data = data.frame(Method = rep("MCP", nrow(df.g)),
                                                            Group = rep(grp[g], nrow(df.g)),
                                                            x = df.g[, x], y = df.g[, y]))
    # set column names to the input values
    names(spts.tmp)[3:4]<- c(x, y)
    # measure distance of each observation to centroid and append to data.frame
    df.g$Dist <- apply(df.g[ , c(x, y)], 1, FUN = function(p){euc.dist(p, cent)})
    # loop through each level
    sp.tmp<- createSPDF() # use helper function
    for(lev in 1:length(levels)){
      # filter rows which meet MCP level threshold distance from centroid
      df.xy<- df.g[which(df.g$Dist <= stats::quantile(df.g$Dist, levels[lev] / 100)), c(x, y)]
      # get rows creeating MCP
      df.xy<- df.xy[grDevices::chull(x = df.xy[ , 1], y = df.xy[ , 2]), ]
      # append first row to end to complete the polygon
      df.xy<- rbind(df.xy, df.xy[1, ])
      # create a single spatial polygon
      rstdy<- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(df.xy[ , ]))), ID = lev)))
      rstdy<- sp::SpatialPolygonsDataFrame(rstdy, data = data.frame(Method = "MCP", Group = grp[g], ConfInt = levels[lev], ShapeArea = rstdy@polygons[[1]]@area), match.ID = FALSE)
      sp.tmp<- sp::rbind.SpatialPolygonsDataFrame(sp.tmp, rstdy)
    } # end levels loop
    # add the group polygon to the list of outputs
    spdf.list<- c(spdf.list, sp.tmp)
    # add the group points to the list of outputs
    spts.list<- c(spts.list, spts.tmp)
  }# close group loop
  # describe the polygons
  names(spdf.list)<- grp
  class(spdf.list)<- "estObj"
  # describe the points
  names(spts.list)<- grp
  class(spts.list)<- "estInput"
  # combine the polygons and points
  mcp<- list(estInput = spts.list, estObj = spdf.list)
  attr(mcp, "package")<- "rKIN"
  return(mcp)
}# close function

