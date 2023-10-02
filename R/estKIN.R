#' Estimate Kernel Isotope Niche
#'
#' Calculates the 2D kernel for isotopic values at multiple confidence levels. Returns a list of
#' sf data frames, each list item representing the grouping variable (i.e. species).
#'
#' @param data data.frame object containing columns of isotopic values and grouping variables
#' @param x character giving the column name of the x coordinates
#' @param y character giving the column name of the y coordinates
#' @param h character describing the bandwidth estimator method. Default = "ref". See Details for more information.
#' @param hval numeric vector of length 2 describing the bandwidth in x and y directions. Default = NULL
#' @param group character giving the column name of the grouping variable (i.e. species)
#' @param levels Numeric vector of desired percent levels (e.g. c(10, 50, 90). Should not be less than 1 or greater than 99)
#' @param scaler numeric value to expand the min/max x and y values. This assists with error given smaller sample sizes. Default value = 10
#' @param smallSamp logical value indicating whether to override minimum number of samples. Currently 10 samples are required.
#' @details Details
#' For the h argument there are 8 different bandwidth estimation options ("hns", "hpi", "hscv", "hlscv", "hbcv", "hnm", "hucv", "ref").
#' "ref" = The default MASS::kde2d bandwidth method. The remaining options are obtained from the 'ks' package with the default
#' method being "hpi". For all ks package methods, the default values are accepted and only the x and y values are passed to the
#' bivariate bandwidth estimating functions. For all bandwidth estimation methods, reducing the data to an individual group will provide the same bandwidths as used during rKIN estimation.
#'
#' * hpi  - Default Plug-in bandwidth selector using ks::Hpi function. Values can be obtained using bw_hpi().
#' * hns  - Normal scale bandwidth using ks::Hns function.Values can be obtained using bw_hns().
#' * hscv - Smoothed cross-validation bandwidth selector. Values can be obtained using bw_hscv().
#' * hlscv - Least-squares cross-validation bandwidth matrix selector for multivariate data. Values can be obtained using bw_hlscv().
#' * hbcv - Biased cross-validation bandwidth matrix selector for bivariate data. Values can be obtained using bw_hbcv().
#' * hnm - Normal mixture bandwidth. Values can be obtained using bw_hnm().
#' * hucv - Least-squares cross-validation bandwidth matrix selector for multivariate data. Values can be obtained using bw_hucv().
#' * ref - Uses MASS::bandwidth.nrd for both x and y separately, dividing values by 4 to match the scale of ks methods. Values can be obtained using bw_ref(). See MASS:kde2d() for details (i.e. the function divides the values by 4).
#'
#'
#' @return A class rKIN object containing a list of sf data frames, each list item representing the grouping variable.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @import ks
#' @import sf
#' @examples
#' library(rKIN)
#' data("rodents")
#' #estimate niche overlap between 2 species using kernel UD
#' test.kin<- estKIN(data=rodents, x="Ave_C", y="Ave_N", group="Species",
#'                  levels=c(50, 75, 95), scaler=2)
#' #determine polygon overlap for all polygons
#' plotKIN(test.kin, scaler=2, title="Kernel Estimates", xlab="Ave_C", ylab="Ave_N")


estKIN <- function(data, x, y, h = "ref", hval = NULL, group, levels = c(50, 75, 95), scaler = 10, smallSamp = FALSE){
  # need to perform some class testing first before running any below code
  if(!inherits(data, "data.frame"))
    stop("data must be a data.frame!")
  if(!inherits(x, "character"))
    stop("x must be a character giving the x coordinate column name!")
  if(x %in% names(data) == FALSE)
    stop("The value of x does not appear to be a valid column name!")
  if(!inherits(data[, x], "numeric"))
    stop("data in column x is not numeric!")
  if(!inherits(y, "character"))
    stop("y must be a character giving the y coordinate column name!")
  if(y %in% names(data) == FALSE)
    stop("The value of y does not appear to be a valid column name!")
  if(!inherits(data[, y], "numeric"))
    stop("data in column y is not numeric!")
  if(!inherits(group, "character"))
    stop("group must be a character giving the grouping variable column name!")
  if(group %in% names(data) == FALSE)
    stop("The value of group does not appear to be a valid column name!")
  if(!inherits(levels, "numeric"))
    stop("levels must be a numeric vector with values ranging between 1 and 100!")
  if(!all(levels > 0 | levels <= 100))
    stop("levels must be a numeric vector with values ranging between 1 and 100!")
  if(!h %in% c("hns", "hpi", "hscv", "hlscv", "hbcv", "hnm", "hucv", "ref") & is.null(hval))
    stop("The bandwidth estimator method is misspecified. Please refer to the list of available options")
  if(!is.null(hval) & length(hval) != 2)
    stop("The provided bandwidth vector (hval) is not of length 2.")
  if(!is.null(hval) & !inherits(hval, "numeric"))
    stop("The provided bandwidth vector (hval) is not numeric.")


  # set the grid size for all groups, expand values by 2 by default
  grid.x<- seq(from = round((min(data[ , x]) - scaler), 1),
               to = round((max(data[ , x]) + scaler), 1), by = 0.1)
  grid.y<- seq(from = round((min(data[ , y]) - scaler), 1),
               to = round((max(data[ , y]) + scaler), 1), by = 0.1)

  # Loop through each unique value of the group column
  grp<- unique(as.character(data[, group]))
  #create the output object for SpatialPolygonsDataFrame(s)
  #spdf.list<- list()
  # create the output object for SpatialPointsDataFrame(s)
  #spts.list<- list()
  sf.tmp<- createSPDF()
  for(g in 1:length(grp)){
    # Test for the number of samples. If too small, kick an error
    if(nrow(data[data[, group] == grp[g] , ]) < 10 & smallSamp == FALSE)
      stop(paste("It appears that group ", grp[g], " has fewer than 10 samples. Please remove group ", grp[g], " from the data.frame."))
    if(nrow(data[data[, group] == grp[g] , ]) < 3 & smallSamp == TRUE)
      stop(paste("It appears that group ", grp[g], " has fewer than 3 samples. Please remove group ", grp[g], " from the data.frame."))
    # Determine the bandwidth using either provided vector or the chosen estimator method
    if(is.null(hval)){
      # Determine the bandwidth given user selected option
      if(h == "hns"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hns(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "hpi"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hpi(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "hscv"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hscv(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "hlscv"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hlscv(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "hbcv"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hbcv(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "hnm"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hnm(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "hucv"){
        # The kde2d function scales the band width by a value of 4, adjusting ks methods to same scale
        band <- (bw_hucv(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
      if(h == "ref"){
        band <- (bw_ref(as.matrix(data[data[, group] == grp[g], c(x, y)]))) * 4
      }
    } else{
      # User provided bandwidth
      band <- hval
    }

    #Estimate 2D kernel of isotope space
    kde<- MASS::kde2d(x = data[data[,group]==grp[g] , x], y = data[data[,group]==grp[g] , y],
                      n = c(length(grid.x), length(grid.y)), h = band,
                      lims = c(min(grid.x), max(grid.x), min(grid.y), max(grid.y)))
    # Must determine quantile thresholds given input levels, default is 50%, 75%, 95%. This is using helper function
    rq<- getKernelThreshold(kde$z, levels)

    df.g<- data[data[, group] == grp[g] , ]


    df.tmp <- data.frame(Method = rep("Kernel", nrow(df.g)),
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

    for(lev in 1:length(levels)){
      cL <- grDevices::contourLines(x = kde$x, y = kde$y, z = kde$z, levels = rq$Threshold[lev])

      # Function was directly copied from raster package
      .contourLines2LineList_sf <- function(cL) {
        n <- length(cL)
        res <- vector(mode = "list", length = n)
        for (i in 1:n) {
          crds <- cbind(cL[[i]][[2]], cL[[i]][[3]])

          res[[i]] <- sf::st_linestring(x = crds)
        }
        res
      }

      if (length(cL) < 1)
        stop("no contour lines")
      cLstack <- tapply(1:length(cL), sapply(cL, function(x) x[[1]]),
                        function(x) x, simplify = FALSE)

      #df <- data.frame(ConfInt = levels[lev])

      m <- length(cLstack)

      res <- vector(mode = "list", length = m)
      #IDs <- paste("C", 1:m, sep = "_")
      #row.names(df) <- IDs

      res <- sf::st_sfc(.contourLines2LineList_sf(cL))

      res <- sf::st_cast(res, to = "POLYGON")

      # vertices <- matrix(c(0, 0, 4, 2, 3, 4, 0, 0), ncol = 2, byrow = TRUE)
      # triangle <- sf::st_polygon(list(vertices))
      # triangle_sf <- sf::st_sfc(triangle)
      #
      # vertices2 <- matrix(c(1.5, 1.5, 2, 2.5, 3, 2.5, 1.5, 1.5), ncol = 2, byrow = TRUE)
      # triangle2 <- sf::st_polygon(list(vertices2))
      # triangle_sf2 <- sf::st_sfc(triangle2)
      #
      # innerVertices <- matrix(c(-26.5, 4, -24, 4.5, -23.5, 5.5, -26.5, 4), ncol = 2, byrow = TRUE)
      # innerTriangle <- sf::st_polygon(list(innerVertices))
      # innerTriangle_sf <- sf::st_sfc(innerTriangle)
      # plot(innerTriangle_sf)

      #res <- c(res, triangle_sf, innerTriangle_sf, triangle_sf2)

      if (length(res) > 1) {
        # find all the outer polygons containing other polygons
        testContains <- sf::st_contains_properly(res)

        # get the index of these outer polygons
        nonemptyIndex <- which(lengths(testContains) > 0)
        if (length(nonemptyIndex) > 0) {

          # erase represents the inner polygon we want to remove
          erase <- sf::st_union(res[unlist(testContains[nonemptyIndex])])
          # erased should be the outer polygon without the inners
          res <- sf::st_difference(res, erase)
        }
      }

      ## Union erased together to smash into sf object that will be sent for CI and group
      outerPolys <- sf::st_union(res)

      sfObj <- sf::st_as_sf(cbind(data.frame(Method = "Kernel", Group = grp[g], ConfInt = levels[lev], ShapeArea = NA_real_), outerPolys))
      # Not accessing the specific geometry column is this okay?
      sfObj$ShapeArea <- sf::st_area(outerPolys)

      sf.tmp <- rbind(sf.tmp, sfObj)

    }# end levels loop

  }# close group loop
  # describe the polygons
  kud<- list(estInput = sfpts.tmp, estObj = sf.tmp)
  attr(kud, "package")<- "rKIN"
  return(kud)
} # close function




