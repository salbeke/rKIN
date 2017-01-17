#' Plotting function for rKIN polygons
#'
#' Using ggplot2 methods, simultaneously plot all of the groups and levels of niche space
#'
#' @param estObj list object created from estKIN, estMCP or estEllipse functions
#' @param scaler numeric value indicating number of isotopic units to expand the x and y axes of the plot. Default is 1.
#' @param alpha numeric value between 0 and 1, representing the amount of transparency of each polygon. 0 is transparent, 1 is opaque.
#' @param title character string for a plot title.
#' @param xlab character or expression string for the x-axis label.
#' @param ylab character or expression string for the y-axis label.
#' @return A plot of all groups and levels.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @import maptools
#' @import rgeos
#' @import ggplot2
#' @examples
#' library(rKIN)
#' data("rodents")
#' #estimate niche overlap between 2 species using kernel UD
#' test.kin<- estKIN(data=rodents, x="Ave_C", y="Ave_N", group="Species",
#'                    levels=c(50, 75, 95), scaler=2)
#' #determine polygon overlap for all polygons
#' plotKIN(test.kin, scaler = 1, title = "Kernel Estimates", xlab = expression({delta}^13*C~ ('\u2030')), ylab = expression({delta}^15*N~ ('\u2030')))

plotKIN<- function(estObj, scaler = 1, alpha = 0.3, title = "", xlab = "x", ylab = "y"){
  requireNamespace("maptools")
  #requireNamespace("rgeos")
  #library(maptools)
  #//////////////////////////
  # NEED TO CHECK FOR PROPER OBJECT TYPES
  if(!inherits(estObj$estObj, "estObj"))
    stop("estObj must be of class estObj created from estEllipse, estKIN, or estMCP functions!")
  if(!inherits(scaler, "numeric"))
    stop("scaler must be numeric!")
  if(!inherits(alpha, "numeric"))
    stop("alpha must be numeric!")
  if(alpha > 1 | alpha < 0)
    stop("alpha must be a numeric value between 0 and 1!")
  if(!inherits(title, "character"))
    stop("title must be a character!")
  if( !inherits(xlab, "character"))
    if(!inherits(xlab, "expression"))
      stop("xlab must be a character or an expression!")
  if(!inherits(ylab, "character"))
    if(!inherits(ylab, "expression"))
      stop("ylab must be a character or an expression!")

  # Get the ConfInt to be sorted descending for ggplot stuff
  ord<- unique(estObj$estObj[[1]]@data$ConfInt)[order(unique(estObj$estObj[[1]]@data$ConfInt), decreasing = TRUE)]
  #get the min/max extent of all SPDF
  xs<- numeric()
  ys<- numeric()
  df<- list()
  #Loop through the polygons
  for(i in 1:length(estObj$estObj)){
    #xs<- c(xs, sp::bbox(estObj$estObj[[i]])[1, ])
    #ys<- c(ys, sp::bbox(estObj$estObj[[i]])[2, ])
    # Create new column to set the drawing order in ggplot, largest CI first
    for(j in 1:length(ord)){
      estObj$estObj[[i]]@data$PlotOrder[estObj$estObj[[i]]@data$ConfInt==ord[j]]<- j
    }# close j loop
    gdf<- ggplot2::fortify(estObj$estObj[[i]], region = "PlotOrder")
    gdf<- merge(gdf, estObj$estObj[[i]]@data, by.x = "id", by.y = "PlotOrder")
    gdf$Group_ConfInt<- paste(gdf$Group, gdf$ConfInt, sep = "_")
    df<- c(df, list(gdf))
  }# close i loop
  #loop through the input points
  pts<- data.frame()
  for(i in 1:length(estObj$estInput)){
    #place all points into one data.frame for plotting
    pts<- rbind(pts, estObj$estInput[[i]]@data)
  }# close i loop
  xs<- pts[ , 3]
  ys<- pts[ , 4]

  # make a plot using ggplot2
  kin.plot<- ggplot2::ggplot() +
    lapply(df, function(x) ggplot2::geom_polygon(data = x, alpha = alpha, ggplot2::aes_string(x = "long", y = "lat", fill = "Group_ConfInt", group = "group"))) +
    scale_fill_manual(values=getColors(length(df), length(ord))) +
    ggplot2::geom_point(data = pts, aes_string(x = names(pts)[3], y = names(pts)[4], colour = "Group", shape = "Group")) +
    ggplot2::scale_color_manual(values = getColors(length(df), 1)) +
    ggplot2::coord_fixed(ratio = ((max(xs) + scaler) - (min(xs) - scaler))/((max(ys) + scaler) - (min(ys) - scaler)),
                xlim = c((min(xs) - scaler), (max(xs) + scaler)),
                ylim = c((min(ys) - scaler), (max(ys) + scaler))) +
    ggplot2::scale_x_continuous(breaks = seq(from = round((min(xs) - scaler)), to = round((max(xs) + scaler)), by = scaler)) +
    ggplot2::scale_y_continuous(breaks = seq(from = round((min(ys) - scaler)), to = round((max(ys) + scaler)), by = scaler)) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = alpha))) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border= ggplot2::element_rect(fill = NA, color = "black"))
  # return the plot
  return(kin.plot)

}# close function



