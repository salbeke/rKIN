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
  # Since RColorBrewer is picky about the number of colors, perform some checks
  if(levels == 1){
    orange<- RColorBrewer::brewer.pal(3, "Oranges")[3]
    purple<- RColorBrewer::brewer.pal(3, "Purples")[3]
    green<- RColorBrewer::brewer.pal(3, "Greens")[3]
    red<- RColorBrewer::brewer.pal(3, "Reds")[3]
    blue<- RColorBrewer::brewer.pal(3, "Blues")[3]
    black<- RColorBrewer::brewer.pal(3, "Greys")[3]
  }
  if(levels == 2){
    orange<- rev(RColorBrewer::brewer.pal(3, "Oranges")[c(1, 3)])
    purple<- rev(RColorBrewer::brewer.pal(3, "Purples")[c(1, 3)])
    green<- rev(RColorBrewer::brewer.pal(3, "Greens")[c(1, 3)])
    red<- rev(RColorBrewer::brewer.pal(3, "Reds")[c(1, 3)])
    blue<- rev(RColorBrewer::brewer.pal(3, "Blues")[c(1, 3)])
    black<- rev(RColorBrewer::brewer.pal(3, "Greys")[c(1, 3)])
  }
  if(levels == 3){
    orange<- rev(RColorBrewer::brewer.pal(4, "Oranges")[c(2:4)])
    purple<- rev(RColorBrewer::brewer.pal(4, "Purples")[c(2:4)])
    green<- rev(RColorBrewer::brewer.pal(4, "Greens")[c(2:4)])
    red<- rev(RColorBrewer::brewer.pal(4, "Reds")[c(2:4)])
    blue<- rev(RColorBrewer::brewer.pal(4, "Blues")[c(2:4)])
    black<- rev(RColorBrewer::brewer.pal(4, "Greys")[c(2:4)])
  }
  if(levels > 3){
    orange<- rev(RColorBrewer::brewer.pal(levels, "Oranges"))
    purple<- rev(RColorBrewer::brewer.pal(levels, "Purples"))
    green<- rev(RColorBrewer::brewer.pal(levels, "Greens"))
    red<- rev(RColorBrewer::brewer.pal(levels, "Reds"))
    blue<- rev(RColorBrewer::brewer.pal(levels, "Blues"))
    black<- rev(RColorBrewer::brewer.pal(levels, "Greys"))
  }

  #orange<- c("#8c2d04", "#d94801", "#f16913", "#fd8d3c", "#fdae6b", "#fdd0a2", "#feedde")
  #purple<- c("#4a1486", "#6a51a3", "#807dba", "#9e9ac8", "#bcbddc", "#dadaeb", "#f2f0f7")
  #green<- c("#005a32", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0", "#edf8e9")
  #red<- c("#99000d", "#cb181d", "#ef3b2c", "#fb6a4a", "#fc9272", "#fcbba1", "#fee5d9")
  #blue<- c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff")
  #black<- c("#252525", "#525252", "#737373", "#969696", "#bdbdbd", "#d9d9d9", "#f7f7f7")
  colorList<- list(orange, purple, green, red, blue, black, orange, purple, green, red, blue, black)
  #create vector to hold ordered colors
  fill<- character()
  for(i in 1:groups){
    fill<- c(fill, colorList[[i]])
    # fill<- c(fill, colorList[[i]][1:levels])
  }
  return(fill)
}
