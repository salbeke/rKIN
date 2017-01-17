#' Method to extract Niche Polygon Areas
#' 
#' Extracts the polygon area for an rKIN object for each group and level.
#' 
#' @param estObj List object created from estKIN, estMCP or estEllipse functions
#' @return A data.frame() of polygon areas.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @examples
#' data("rodents")
#' #estimate niche overlap between 2 species using kernel UD
#' test.kin<- estKIN(data=rodents, x="Ave_C", y="Ave_N", group="Species", 
#'                  levels=c(50, 75, 95), scaler=2)
#' #determine polygon sizes/areas
#' kin.area<- getArea(test.kin)

getArea<- function(estObj){
  if(!inherits(estObj$estObj, "estObj"))
    stop("estObj must be of class estObj created from estEllipse, estKIN, or estMCP functions!")
  
  outDF<- data.frame()
  # loop through the list
  for(i in 1:length(estObj$estObj)){
    outDF<- rbind(outDF, estObj$estObj[[i]]@data)
  }
  return(outDF)
}