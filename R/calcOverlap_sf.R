calcOverlap_sf <- function(estObj){
  # need to check the class of input object
  #if(!inherits(estObj$estObj, "estObj"))
    #stop("estObj must be of class estObj created from estEllipse, estKIN, or estMCP functions!")

  # get number of levels
  #lev<- unique(estObj$estObj[[1]]@data[,"ConfInt"])
  lev <- unique(estObj$estObj$ConfInt)
  #create row/col name vector
  nm<- matrix(, nrow = 0, ncol = 2)
  # for(n in names(estObj$estObj)){
  #   for(l in lev){
  #     nm<- rbind(nm, matrix(c(n, l), ncol = 2))
  #   }
  # }


  for(n in unique(estObj$estObj$Group)){
    for(l in lev){
      nm<- rbind(nm, matrix(c(n, l), ncol = 2))
    }
  }

  #create a data.frame to store the overlap values
  df<- data.frame(OverlapID = apply(nm, 1, FUN = function(b) paste(b[1], b[2], sep = "_")))
  df[, apply(nm, 1, FUN = function(b) paste(b[1], b[2], sep = "_"))]<- 0
  # loop through and calculate overlap between each polygon.
  # Row is the first polygon, col is the 2nd polygon

  #intersections <- sf::st_intersection(estObj$estObj[1,], estObj$estObj)
  # for (i in 1:nrow(nm)) {
  #   intersections <- sf::st_intersection(estObj$estObj[i,], estObj$estObj)
  #
  #   #df[i + 1]<- ifelse(is.null(g.int), 0, round((sf::st_area(g.int) / sf::st_area(rpoly)), 3))
  #   df[i + 1] <- round((sf::st_area(intersections$geometry) / intersections$ShapeArea.1), 3)
  #
  # }


  for(i in 1:nrow(nm)){
    # get the row polygon
    # gets the group
    #g<- which(unique(estObj$estObj$Group)==nm[i, 1])
    # get the level for the chosen group
    #rpoly<- estObj$estObj[[g]][which(estObj$estObj[[g]]@data$ConfInt==nm[i, 2]), ]
    rpoly<- estObj$estObj |>
      dplyr::filter(Group == nm[i,1]) |>
      dplyr::filter(ConfInt == nm[i,2])
    # loop through the col polygons and intersect
    for(j in 1:nrow(nm)){
      # gets the group
      #g.c<- which(names(estObj$estObj)==nm[j, 1])
      #g.c<- which(unique(estObj$estObj$Group)==nm[j, 1])
      # get the level for the chosen group
      #cpoly<- estObj$estObj[[g.c]][which(estObj$estObj[[g.c]]@data$ConfInt==nm[j, 2]), ]
      cpoly <- estObj$estObj |>
        dplyr::filter(Group == nm[j,1]) |>
        dplyr::filter(ConfInt == nm[j,2])
      #g.int<- rgeos::gIntersection(rpoly, cpoly)

      sf::st_agr(rpoly) = "constant"
      sf::st_agr(cpoly) = "constant"
      g.int <- sf::st_intersection(rpoly, cpoly)
      #df[i,(j+1)]<- ifelse(is.null(g.int), 0, round((rgeos::gArea(g.int) / rgeos::gArea(rpoly)), 3))
      df[i,(j+1)]<- ifelse(nrow(g.int) == 0, 0, round((sf::st_area(g.int) / sf::st_area(rpoly)), 3))
    }# close j loop
  }# close i loop
  return(df)
}# close function
