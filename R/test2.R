# Coordinates for Species1
species1_x <- c(0.5, 1, 1.5, 2, 2, 1.5, 1, 0.5)
species1_y <- c(0, 0.5, 1, 1, 2, 2.5, 2, 0)

# Coordinates for Species2
species2_x <- c(2.5, 3, 3.5, 4, 4, 3.5, 3, 2.5)
species2_y <- c(0, 0.5, 1, 1, 2, 2.5, 2, 0)



sampleEstDataframe <- data.frame(Species = c(rep("Species1", length(species1_x)), rep("Species2", length(species2_x))),
                 Ave_C = c(species1_x, species2_x),
                 Ave_N = c(species1_y, species2_y))



species1_sf <- st_sf(geometry = st_sfc(st_polygon(list(cbind(species1_x, species1_y)))))
species2_sf <- st_sf(geometry = st_sfc(st_polygon(list(cbind(species2_x, species2_y)))))

df <- bind_rows(species1_sf, species2_sf)


names(df) <- c("Species", "Ave_C", "Ave_N")

# Populate the Ave_C and Ave_N columns
df$Ave_C <- c(species1_x, species2_x)
df$Ave_N <- c(species1_y, species2_y)
plot(df)
