#///////////////////////////////////////////////////////////
#' Internal helper function
#'
#' Create empty sf data frame with estObj schema
#'
#' @return An empty sf data frame object matching the expected schema of the estKIN function.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
createSPDF<- function(){
  sp.poly <- sf::st_sfc(sf::st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0)))))
  sp.poly <- sf::st_sf(Method = "rKIN", Group = "1", ConfInt = 0, ShapeArea = 1.1, geom = sp.poly)
  return(sp.poly[-1,])
}
