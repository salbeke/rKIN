#///////////////////////////////////////////////////////////
#' Internal helper function
#'
#' Create empty SpatialPolygonsDataFrame with estObj schema
#'
#' @return An empty SpatialPolygonsDataFrame object matching the expected schema of the estKIN function.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
createSPDF<- function(){
  sp.poly<- sp::Polygon(matrix(c(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0)), ncol = 2))
  sp.poly<- sp::Polygons(list(sp.poly), ID = "1")
  sp.poly<- sp::SpatialPolygons(list(sp.poly))
  sp.poly<- sp::SpatialPolygonsDataFrame(sp.poly, data = data.frame(Method = "rKIN", Group = "1", ConfInt = 0, ShapeArea = 1.1))
  return(sp.poly[-1, ])
}
