#/////////////////////////////////////////////////////////////
#' Miscellaneous functions to complete kernel 2D estimates: make polygon hole when inner circle
#'
#' For multipart polygons, set the inner circle to be 'hole=TRUE' after obtaining contour lines from kernel estimate.
#' The hole is determined using the gContainsProper() function from rgeos package. This is run within the
#' 2D-Kernel estimator function estKIN()
#'
#' @param poly The multipart polygon
#' @param outer The outer ring of the polygon
#' @param hole The inner ring of the polygon
#' @return A Polygons object
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#//////////////////////////////////////////////////////////

makeHole<- function(poly, outer, hole){
  # make the hole truly a hole
  poly@polygons[[hole]]@Polygons[[1]]@hole<- TRUE
  poly@polygons[[hole]]@Polygons[[1]]@ringDir<- as.integer(-1)
  # reverse the coordinate order
  poly@polygons[[hole]]@Polygons[[1]]@coords<- apply(poly@polygons[[hole]]@Polygons[[1]]@coords, 2, FUN = rev)
  # Determine number of Polygons in the slot
  plist<- list()
  for(m in 1:length(poly@polygons[[outer]]@Polygons)){
    plist<- c(plist, poly@polygons[[outer]]@Polygons[[m]])
  }
  # copy the hole poly into the outer polygons slot
  #p<- Polygons(list(poly@polygons[[outer]]@Polygons[[1]], poly@polygons[[hole]]@Polygons[[1]]), ID=poly@polygons[[outer]]@ID)
  p<- sp::Polygons(c(plist, poly@polygons[[hole]]@Polygons[[1]]), ID = poly@polygons[[outer]]@ID)
  poly@polygons[[outer]]<- p
  # now erase the hole
  poly<- poly[-hole,]
  return(poly)
}
