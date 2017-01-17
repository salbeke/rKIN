#/////////////////////////////////////////////////////////////
#' Create a list of colors for plotKIN function
#'
#' The list of colors were obtained from Colorbrewer2.org using single hue.
#' This is run within the function plotKIN()
#' 
#' @param groups The number of groups within grouping variable (i.e. species)
#' @param levels The number of confidence intervals provided by the user
#' @return A character vector of RGB colors 
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#//////////////////////////////////////////////////////////

getColors<- function(groups, levels){
  orange<- c("#8c2d04", "#d94801", "#f16913", "#fd8d3c", "#fdae6b", "#fdd0a2", "#feedde")
  purple<- c("#4a1486", "#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb", "#f2f0f7")
  green<- c("#005a32", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0", "#edf8e9")
  red<- c("#99000d", "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1", "#fee5d9")
  blue<- c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff")
  black<- c("#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9", "#f7f7f7")
  colorList<- list(orange, purple, green, red, blue, black)
  #create vector to hold ordered colors
  fill<- character()
  for(i in 1:groups){
    fill<- c(fill, colorList[[i]][1:levels])
  }
  return(fill)
}