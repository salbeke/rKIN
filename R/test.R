library(rKIN)
exampleEstObj <- rKIN::estMCP(data, x, y, group, levels, smallSamp)

exampleSfEstObj <- estMCP_sf(data, x, y, group, levels, smallSamp)

testWithExmpDf <- estMCP_sf(sampleEstDataframe, x, y, group, levels, smallSamp)
testCalcOutput <- calcOverlap_sf(exampleSfEstObj)


calcSfOutput <- calcOverlap_sf(exampleSfEstObj)
calcOutput <- rKIN::calcOverlap(exampleEstObj)
estObj <- exampleSfEstObj


testIntersect <- sf::st_intersection(exampleSfEstObj$estObj[1,], exampleSfEstObj$estObj)

sf::st_area(testIntersect$geometry)



# Create the x and y coordinates for Species 1 and Species 2
species1_x <- c(0.5, 1, 1.5, 2, 2, 1.5, 1, 0.5)
species1_y <- c(0, 0.5, 1, 1, 2, 2.5, 2, 1.5)

species2_x <- c(2.5, 3, 3.5, 4, 4, 3.5, 3, 2.5)
species2_y <- c(0, 0.5, 1, 1, 2, 2.5, 2, 1.5)


library(sf)

species1_sf <- st_sf(geometry = st_sfc(st_polygon(list(cbind(species1_x, species1_y)))))
species2_sf <- st_sf(geometry = st_sfc(st_polygon(list(cbind(species2_x, species2_y)))))
# Combine the coordinates into a data frame
nonOverlapping <- data.frame(Species = c(rep("Species1", length(species1_x)), rep("Species2", length(species2_x))),
                 Ave_C = c(species1_x, species2_x),
                 Ave_N = c(species1_y, species2_y))

# Print the resulting data frame
nonOverlapping


nonOverlappingEstObj <- estMCP_sf(species_df, x, y, group, levels, smallSamp)


# Create points for Species 1
set.seed(123)
x1 <- rnorm(20, mean = 4, sd = 0.5)
y1 <- rnorm(20, mean = 4, sd = 0.5)

# Create points for Species 2
set.seed(321)
x2 <- rnorm(20, mean = 8, sd = 0.5)
y2 <- rnorm(20, mean = 8, sd = 0.5)

# Combine x and y coordinates for each species into a data frame
species1_df <- data.frame(x = x1, y = y1, Species = "Species1")
species2_df <- data.frame(x = x2, y = y2, Species = "Species2")
species_df <- bind_rows(species1_df, species2_df)



species_sf <- sf::st_as_sf(species_df, coords = c("x", "y"), crs = 4326)

# Create polygons for each species
species_polygons <- species_sf %>%
  group_by(Species) %>%
  summarise(geometry = sf::st_combine(sf::st_convex_hull(sf::st_collect(geometry))))

# Plot polygons
ggplot() +
  geom_sf(data = species_polygons, aes(fill = Species), alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_void()

names(species_df) <- c("Ave_C", "Ave_N", "Species")


calcNonOverlap <- calcOverlap_sf(testWithExmpDf)


tmp<- testWithExmpDf$estObj
testWithExmpDf$estObj <- tmp
tmp<- tmp[c(1,4),]
plot(tmp$geometry)

lev <- unique(tmp$ConfInt)
#create row/col name vector
nm<- matrix(, nrow = 0, ncol = 2)
# for(n in names(estObj$estObj)){
#   for(l in lev){
#     nm<- rbind(nm, matrix(c(n, l), ncol = 2))
#   }
# }


for(n in unique(tmp$Group)){
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
for (i in 1:nrow(nm)) {

  for (j in 1:nrow(nm)) {

  }
  intersections <- sf::st_intersection(tmp[i,], tmp[2,])
  nrow(intersections) == 0
  #df[i + 1]<- ifelse(is.null(g.int), 0, round((sf::st_area(g.int) / sf::st_area(rpoly)), 3))
  #df[i + 1] <- round((sf::st_area(intersections$geometry) / intersections$ShapeArea.1), 3)

}




ellipseSfOutput <- estEllipse_sf(data, x, y, group, levels, smallSamp)

ogEllipseOutput <- rKIN::estEllipse(data, x, y, group, levels, smallSamp)

plotKIN(ogEllipseOutput)

gg

length(ogEllipseOutput$estObj)
length(ellipseSfOutput$estInput)

sf::st_bbox(ellipseSfOutput$estObj)

ellipseSfOutput$estObj[order(ellipseSfOutput$estObj$ConfInt, decreasing = TRUE),]

ellipseSfOutput$estObj$Group_ConfInt <- paste(ellipseSfOutput$estObj$Group, gdf$ConfInt, sep = "_")
sfxs <- numeric()
sfys <- numeric()
groupVars <- unique(ellipseSfOutput$estObj$Group)
for (i in 1:length(groupVars))
{
  bounding <- sf::st_bbox(ellipseSfOutput$estObj[ellipseSfOutput$estObj$Group == groupVars[i],]$geometry)
  sfxs <- c(sfxs, bounding["xmin"])
  sfxs <- c(sfxs, bounding["xmax"])

  sfys <- c(sfys, bounding["ymin"])
  sfys <- c(sfys, bounding["ymax"])
}

grp1 <- ellipseSfOutput$estObj[ellipseSfOutput$estObj$Group == "Species1",]$geometry
grp2 <- ellipseSfOutput$estObj[ellipseSfOutput$estObj$Group == "Species2",]
sfxs <- numeric()
sfxs <- c(sfxs, sf::st_bbox(grp1$geometry)["xmin"])
sfxs <- c(sfxs, sf::st_bbox(grp1$geometry)["xmax"])

sfxs <- c(sfxs, ellipseSfOutput$estInput$Ave_C)
sfys <- c(sfys, ellipseSfOutput$estInput$Ave_N)

# Set the x and y axes limits
ifelse(is.null(xmin) & !is.numeric(xmin), xmin <- (min(sfxs) - scaler), xmin)
ifelse(is.null(xmax) & !is.numeric(xmax), xmax <- (max(sfxs) + scaler), xmax)
ifelse(is.null(ymin) & !is.numeric(ymin), ymin <- (min(sfys) - scaler), ymin)
ifelse(is.null(ymax) & !is.numeric(ymax), ymax <- (max(sfys) + scaler), ymax)

ellipseSfOutput$estObj$Group_ConfInt <- paste(ellipseSfOutput$estObj$Group, ellipseSfOutput$estObj$ConfInt, sep = "_")

sfgrps <- length(unique(ellipseSfOutput$estObj$Group))
sflvls <- length(unique(ellipseSfOutput$estObj$ConfInt))
for (i in 1:length(unique(ellipseSfOutput$estObj$Group))) {

}
ggplot2::ggplot() +
  ggplot2::geom_sf(data = ellipseSfOutput$estObj[order(ellipseSfOutput$estObj$ConfInt, decreasing = TRUE),], ggplot2::aes(fill = Group_ConfInt, group = "ShapeArea"), color = "transparent", alpha = 0.3, size = 10) +
  ggplot2::scale_fill_manual(values = getColors(sfgrps, sflvls)) +
  ggplot2::geom_sf(data = ellipseSfOutput$estInput, aes(color = Group, shape = Group)) +
  ggplot2::scale_color_manual(values = getColors(sfgrps, 1)) +
  ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  ggplot2::scale_x_continuous(breaks = seq(from = round(xmin), to = round(xmax), by = scaler)) +
  ggplot2::scale_y_continuous(breaks = seq(from = round(ymin), to = round(ymax), by = scaler)) +
  ggplot2::labs(title = title, x = xlab, y = ylab) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                 plot.title = element_text(hjust = 0.5),
                 aspect.ratio = 1.0)

class(ellipseSfOutput$estInput)
  #
  #ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))


