#/////////////////////////////////////////////////////////////
#' Create a list of colors for plotKIN function
#'
#' The list of colors were obtained from Colorbrewer2.org using single hue.
#' This is run within the function plotKIN()
#'
#' @param groups The number of groups within grouping variable (i.e. species)
#' @param levels The number of confidence intervals provided by the user
#' @param colors Character vector of hex codes representing desired colors
#' @return A character vector of RGB colors
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#//////////////////////////////////////////////////////////

getColors<- function(groups, levels, colors = NULL){
  # Since RColorBrewer is picky about the number of colors, perform some checks
  # All the color palettes being used are sequential and only have up to 9 different values
  # Thus if someone has more than 9 levels stop the program
  if (levels > 9)
    stop("Maximum allowed levels is 9, sorry")

  fill<- character()
  if (!is.null(colors)) {
    # First check that the users colors are in the correct format
    for (element in colors) {
      first_char <- substr(element, 1, 1)
      if (first_char != "#") {
        stop("All custom colors provided must be hex codes representing the desired color starting with a '#'")
      }
    }

    if (groups != length(colors)) {
      stop("The number of groups does not equal the number of colors provided. Please make sure you provide the exact amount of colors for how many groups you have.")
    }

    colorList <- colors
    if (levels > 1 ) {
      for (i in 1:length(colorList)) {
<<<<<<< HEAD
        indivLevels <- character()
=======
        #indivLevels <- character()
>>>>>>> development
        indivLevels <- colorList[i]
        for (j in 2:levels) {
          indivLevels <- c(indivLevels, shades::brightness(indivLevels[j-1], 0.5))
        }
        fill <- c(fill, indivLevels)
      }
    }
    else {
      fill <- colors
    }

  }
<<<<<<< HEAD

  if (groups > 6) {
=======
  else if (groups > 6) {
>>>>>>> development
    # use rcolorbrewer colors for up to 6 then use random colors
    orange<- RColorBrewer::brewer.pal(3, "Oranges")[3]
    purple<- RColorBrewer::brewer.pal(3, "Purples")[3]
    green<- RColorBrewer::brewer.pal(3, "Greens")[3]
    red<- RColorBrewer::brewer.pal(3, "Reds")[3]
    blue<- RColorBrewer::brewer.pal(3, "Blues")[3]
    black<- RColorBrewer::brewer.pal(3, "Greys")[3]
    randomColors <- randomcoloR::randomColor(count = (groups - 6))
    #randomColors <- randomcoloR::randomColor(count = groups, luminosity = "dark")
    colorList<- list(orange, purple, green, red, blue, black)
    colorList <- c(colorList, randomColors)
    #colorList<- randomColors
    brightnessRatio <- 0.5/(levels - 1)
    if (levels > 1 ) {
      for (i in 1:length(colorList)) {
        indivLevels <- character()
        indivLevels <- colorList[i]
        for (j in 2:levels) {
          if (j == 2) {
            brightnessRatio <- 0
          }
          else {
            brightnessRatio <- brightnessRatio + 0.5/(levels - 1)
          }
          indivLevels <- c(indivLevels, shades::brightness(indivLevels[j-1], (0.5 + brightnessRatio)))
        }
        fill <- c(fill, indivLevels)
      }
    }
    else {
      fill <- colorList
    }

  }
  else {
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
<<<<<<< HEAD
    colorList<- list(orange, purple, green, red, blue, black, orange, purple, green, red, blue, black)

    #create vector to hold ordered colors

    for(i in 1:groups){
      fill<- c(fill, colorList[[i]])
      # fill<- c(fill, colorList[[i]][1:levels])
=======
    #colorList<- list(orange, purple, green, red, blue, black, orange, purple, green, red, blue, black)
    colorList<- list(orange, purple, green, red, blue, black)

    #create vector to hold ordered colors

    for (i in 1:groups) {
      fill <- c(fill, colorList[[i]][1:levels]) # Select the first 'levels' colors.
>>>>>>> development
    }
  }


<<<<<<< HEAD




=======
>>>>>>> development
  # Need to create a for loop that takes in indiviudal colors and increases brightness,
  # Then need to take increased brightness, and also increase that color for all levels
  #brightness(myColors, 0.9)

  return(fill)
}
