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
#' @param xmin default is NULL, numeric value of user specified minimum x axis value
#' @param xmax default is NULL, numeric value of user specified maximum x axis value
#' @param ymin default is NULL, numeric value of user specified minimum y axis value
#' @param ymax default is NULL, numeric value of user specified maximum y axis value
#' @param colors default is NULL, character vector of hex codes representing colors for plot
#' @return A plot of all groups and levels.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @import sf
#' @import ggplot2
#' @examples
#' library(rKIN)
#' data("rodents")
#' #estimate niche overlap between 2 species using kernel UD
#' test.kin<- estKIN(data=rodents, x="Ave_C", y="Ave_N", group="Species",
#'                    levels=c(50, 75, 95), scaler=2)
#' #determine polygon overlap for all polygons
#' plotKIN(test.kin, scaler = 1, title = "Kernel Estimates",
#'          xlab = expression({delta}^13*C~ ('Per Mille')),
#'          ylab = expression({delta}^15*N~ ('Per Mille')))

plotKIN<- function(estObj, scaler = 1, alpha = 0.3, title = "", xlab = "x", ylab = "y", xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, colors = NULL){

  # NEED TO CHECK FOR PROPER OBJECT TYPES
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
  if(!inherits(xmin, "numeric"))
    if(!is.null(xmin))
      stop("xmin must be numeric or NULL!")
  if(!inherits(xmax, "numeric"))
    if(!is.null(xmax))
      stop("xmax must be numeric or NULL!")
  if(!inherits(ymin, "numeric"))
    if(!is.null(ymin))
      stop("ymin must be numeric or NULL!")
  if(!inherits(ymax, "numeric"))
    if(!is.null(ymax))
      stop("ymax must be numeric or NULL!")


  # Get the ConfInt to be sorted descending for ggplot stuff
  #ord<- unique(estObj$estObj[[1]]@data$ConfInt)[order(unique(estObj$estObj[[1]]@data$ConfInt), decreasing = TRUE)]

  #ord <- unique(ellipseSfOutput$estObj$ConfInt)[order(unique(ellipseSfOutput$estObj$ConfInt), decreasing = TRUE)]
  #get the min/max extent of all SPDF
  xs<- numeric()
  ys<- numeric()
  estObj$estObj$Group_ConfInt <- paste(estObj$estObj$Group, estObj$estObj$ConfInt, sep = "_")
  #df<- list()

  # Get the points for the bounding box
  groupVars <- unique(estObj$estObj$Group)

  # if(length(groupVars) > 6)
  #   stop("You have more than 6 Groups, this is quite a few and plotKIN will currently fail with that many due
  #        to the number of discernable color pallettes. Perhaps try reducing your data to fewer groups?")


  for (i in 1:length(groupVars))
  {
    bounding <- sf::st_bbox(estObj$estObj[estObj$estObj$Group == groupVars[i],]$geometry)
    xs <- c(xs, bounding["xmin"])
    xs <- c(xs, bounding["xmax"])

    ys <- c(ys, bounding["ymin"])
    ys <- c(ys, bounding["ymax"])
  }
  # Get the rest of the points
  coords <- sf::st_coordinates(estObj$estInput$geometry)
  xs <- c(xs, coords[,1])
  ys <- c(ys, coords[,2])


  # Set the x and y axes limits
  ifelse(is.null(xmin) & !is.numeric(xmin), xmin <- (min(xs) - scaler), xmin)
  ifelse(is.null(xmax) & !is.numeric(xmax), xmax <- (max(xs) + scaler), xmax)
  ifelse(is.null(ymin) & !is.numeric(ymin), ymin <- (min(ys) - scaler), ymin)
  ifelse(is.null(ymax) & !is.numeric(ymax), ymax <- (max(ys) + scaler), ymax)


  grps <- length(unique(estObj$estObj$Group))
  lvls <- length(unique(estObj$estObj$ConfInt))
  myColors <- unlist(getColors(grps, lvls, colors))
  baseColors <- character()
  j <- 1
  for (i in 1:grps) {
    baseColors <- c(baseColors, myColors[j])
    j <- j + lvls
  }
  kin.plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = estObj$estObj[order(estObj$estObj$ConfInt, decreasing = TRUE),], ggplot2::aes(fill = .data$Group_ConfInt, group = "ShapeArea"), color = "transparent", alpha = alpha, size = 10) +
    # This is for coloring polygons
    ggplot2::scale_fill_manual(values = myColors) +
    ggplot2::geom_sf(data = estObj$estInput, ggplot2::aes(color = .data$Group, shape = .data$Group)) +
    # This is for coloring data points
    ggplot2::scale_color_manual(values = baseColors) +
    ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    ggplot2::scale_x_continuous(breaks = seq(from = round(xmin), to = round(xmax), by = scaler)) +
    ggplot2::scale_y_continuous(breaks = seq(from = round(ymin), to = round(ymax), by = scaler)) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   aspect.ratio = 1.0)
  # return the plot

  if (grps > 6) {
    kin.plot <- kin.plot + ggplot2::theme(legend.key.size = ggplot2::unit(0.3, 'cm'))
  }
  return(kin.plot)

}# close function



