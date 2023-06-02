estObj <- ogEllipseOutput
scaler = 1
alpha = 0.3
title = ""
xlab = "x"
ylab = "y"
xmin = NULL
xmax = NULL
ymin = NULL
ymax = NULL


plotKIN_sf<- function(estObj, scaler = 1, alpha = 0.3, title = "", xlab = "x", ylab = "y", xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL){
  requireNamespace("maptools")
  #requireNamespace("rgeos")
  #library(maptools)
  #//////////////////////////
  # NEED TO CHECK FOR PROPER OBJECT TYPES
  # if(!inherits(estObj$estObj, "estObj"))
  #   stop("estObj must be of class estObj created from estEllipse, estKIN, or estMCP functions!")
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

  if(length(groupVars) > 6)
    stop("You have more than 6 Groups, this is quite a few and plotKIN will currently fail with that many due
         to the number of discernable color pallettes. Perhaps try reducing your data to fewer groups?")

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

  #Loop through the polygons
  # for(i in 1:length(estObj$estObj)){
  #   xs<- c(xs, sp::bbox(estObj$estObj[[i]])[1, ])
  #   ys<- c(ys, sp::bbox(estObj$estObj[[i]])[2, ])
  #   # Create new column to set the drawing order in ggplot, largest CI first
  #   for(j in 1:length(ord)){
  #     estObj$estObj[[i]]@data$PlotOrder[estObj$estObj[[i]]@data$ConfInt==ord[j]]<- j
  #   }# close j loop
  #   gdf<- ggplot2::fortify(estObj$estObj[[i]], region = "PlotOrder")
  #   gdf<- merge(gdf, estObj$estObj[[i]]@data, by.x = "id", by.y = "PlotOrder")
  #   gdf$Group_ConfInt<- paste(gdf$Group, gdf$ConfInt, sep = "_")
  #   df<- c(df, list(gdf))
  # }# close i loop





  # #loop through the input points
  # pts<- list()
  # for(i in 1:length(estObj$estInput)){
  #   #place all points into data.frame list for plotting
  #   #pf<- ggplot2::fortify(estObj$estInput[[i]], region = "Group")
  #
  #   pts<- c(pts, list(estObj$estInput[[i]]@data))
  #   #store all coordinates for later use
  #   xs<- c(xs, estObj$estInput[[i]]@data[ , 3])
  #   ys<- c(ys, estObj$estInput[[i]]@data[ , 4])
  # }# close i loop

  # Set the x and y axes limits
  ifelse(is.null(xmin) & !is.numeric(xmin), xmin <- (min(xs) - scaler), xmin)
  ifelse(is.null(xmax) & !is.numeric(xmax), xmax <- (max(xs) + scaler), xmax)
  ifelse(is.null(ymin) & !is.numeric(ymin), ymin <- (min(ys) - scaler), ymin)
  ifelse(is.null(ymax) & !is.numeric(ymax), ymax <- (max(ys) + scaler), ymax)


  # make a plot using ggplot2
  # kin.plot<- ggplot2::ggplot() +
  #   lapply(df, function(x) ggplot2::geom_polygon(data = x, alpha = alpha, ggplot2::aes_string(x = "long", y = "lat", fill = "Group_ConfInt", group = "group"))) +
  #   ggplot2::scale_fill_manual(values = getColors(length(df), length(ord))) +
  #   #ggplot2::geom_point(data = pts, aes_string(x = names(pts)[3], y = names(pts)[4], colour = "Group", shape = "Group")) +
  #   lapply(pts, function(x) ggplot2::geom_point(data = x, ggplot2::aes_string(x = names(x)[3], y = names(x)[4], colour = "Group", shape = "Group"))) +
  #   ggplot2::scale_color_manual(values = getColors(length(df), 1)) +
  #   ggplot2::coord_fixed(ratio = (xmax - xmin)/(ymax - ymin), xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  #   ggplot2::scale_x_continuous(breaks = seq(from = round(xmin), to = round(xmax), by = scaler)) +
  #   ggplot2::scale_y_continuous(breaks = seq(from = round(ymin), to = round(ymax), by = scaler)) +
  #   ggplot2::labs(title = title, x = xlab, y = ylab) +
  #   ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = alpha))) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(panel.background = ggplot2::element_blank(),
  #                  panel.grid.major = ggplot2::element_blank(),
  #                  panel.grid.minor = ggplot2::element_blank(),
  #                  panel.border = ggplot2::element_rect(fill = NA, color = "black"),
  #                  plot.title = element_text(hjust = 0.5))


  grps <- length(unique(estObj$estObj$Group))
  lvls <- length(unique(estObj$estObj$ConfInt))

  kin.plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = estObj$estObj[order(estObj$estObj$ConfInt, decreasing = TRUE),], ggplot2::aes(fill = Group_ConfInt, group = "ShapeArea"), color = "transparent", alpha = 0.3, size = 10) +
    ggplot2::scale_fill_manual(values = getColors(grps, lvls)) +
    ggplot2::geom_sf(data = estObj$estInput, ggplot2::aes(color = Group, shape = Group)) +
    ggplot2::scale_color_manual(values = getColors(grps, 1)) +
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
  return(kin.plot)

}# close function
