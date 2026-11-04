##########################################
# INITIALISATION OF GRID CELL ATTRIBUTES #
##########################################

# Set working directory ------------------------------------------------------------------------------------

library(here)
here::here()

library(dplyr)
library(sf)
library(sp)
library(rnaturalearth)
library(lubridate)
library(ggplot2)
library(ggspatial)
library(adehabitatHR)
library(raster)
library(readr)
library(devtools)
library(gridExtra)

# Set coordinate reference system --------------------------------------------------------------------------

wgscrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Import data ----------------------------------------------------------------------------------------------

# GPS tracking data
gps_data <- read.csv2(here("Data", "HMM_Rouzic_Morus_bassanus.csv")) %>%
  dplyr::select(indID, tripID, datetime, longitude, latitude, behaviour) %>%
  dplyr::mutate(datetime = as.POSIXct(strptime(datetime, "%d/%m/%Y %H:%M", tz = "Europe/Paris"))) %>%
  dplyr::filter(!(year(datetime) %in% c(2006, 2022))) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = CRS(wgscrs)) %>%
  sf::st_transform(CRS('EPSG:2154'))

# Coastline
coastline <- sf::st_read(here("Data", "Europe.shp")) %>%
  sf::st_transform(CRS("EPSG:2154"))

# Offshore wind farms
owf <- sf::st_read(here("Data", "all_owf.shp")) %>%
  sf::st_transform(CRS("EPSG:2154")) %>%
  dplyr::select(NAME, geometry)

# Collision risk
col_risks <- read.csv2(here("Data", "collision_risks.csv"))
owf <- left_join(owf, col_risks, by = join_by(NAME == owf))
mean_col_risk <- mean(col_risks$colRisk)
owf$colRisk[is.na(owf$colRisk)] <- mean_col_risk

# Colony location
colony <- data.frame(name = "Rouzic", longitude = -3.436752, latitude = 48.899868) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = CRS(wgscrs)) %>%
  sf::st_transform(CRS('EPSG:2154'))

# Create the model grid ------------------------------------------------------------------------------------

reso <- 5000
buffer <- 1000
xmin <- min(st_coordinates(gps_data)[,"X"]) - buffer
ymin <- min(st_coordinates(gps_data)[,"Y"]) - buffer
xmax <- max(st_coordinates(gps_data)[,"X"]) + buffer
ymax <- max(st_coordinates(gps_data)[,"Y"]) + buffer

grid <- st_make_grid(st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)),
                     cellsize = reso,
                     crs = CRS('EPSG:2154')) %>%
  st_as_sf() %>%
  mutate(centroid.x = st_coordinates(st_centroid(x))[,"X"],
         centroid.y = st_coordinates(st_centroid(x))[,"Y"]) %>%
  arrange(desc(centroid.y)) # We order grid cells so that the first one is at the top left

st_crs(grid) == st_crs(gps_data)
st_crs(grid) == st_crs(colony)
st_crs(grid) == st_crs(coastline)
st_crs(grid) == st_crs(owf)

# Plot to check that everything is ok
# ggplot() +
#   #geom_sf(data = grid, fill = NA) +
#   geom_sf(data = coastline, color = "grey30", fill = "grey70") +
#   geom_path(data = gps_data,
#             aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = indID),
#             linewidth = 0.6) +
#   geom_sf(data = owf, fill = "#FF5E5B", color = "black", alpha = 0.8) +
#   geom_sf(data = colony, size = 2) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_blank()) +
#   coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

# Initialisation of cell attributes ------------------------------------------------------------------------

# Cell attributes are:
  #1/ Cell ID (1 to 10209)
  #2/ Cell use ('land', 'sea', 'colony' or 'owf')
  #3/ Probability of use (probability of moving to this cell from a neighboring cell)
  #4/ Probability of foraging in this cell
  #5/ Collision risk when a gannet spends one time step (6 min) in this cell

## 1/ Cell ID ----------------------------------------------------------------------------------------------

grid$cellID <- 1:nrow(grid)

## 2/ Cell use --------------------------------------------------------------------------------------------

# Identify land cells
landCells <- st_intersection(grid, coastline)
grid$cellUse <- ifelse(grid$cellID %in% landCells$cellID, "land", "sea")

# Identify colony cell
coloCell <- st_intersection(grid, colony)
grid$cellUse[grid$cellID %in% coloCell$cellID] <- "colony"

# Identify OWF cells
owfCells <- st_intersection(grid, owf)
grid$cellUse[grid$cellID %in% owfCells$cellID] <- "owf"

# Specify collision risk for each OWF cell
# Some cells overlap with 2 OWFs with 2 different collision risks: when this is the case, we calculate the mean collision risk
colrisk_cell <- owfCells %>%
  st_drop_geometry() %>%
  group_by(cellID) %>%
  summarise(colRisk = mean(colRisk))
grid <- left_join(grid, colrisk_cell, by = "cellID")

grid$colRisk[is.na(grid$colRisk)] <- 0

# Convert cell use into a numerical variable
grid$cellUse_num <- as.numeric(factor(grid$cellUse))

ggplot() +
  geom_sf(data = grid, aes(fill = colRisk)) +
  geom_sf(data = coastline, color = "grey30", fill = "grey70") +
  geom_sf(data = colony, size = 2) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

## 3/ Probability of use -----------------------------------------------------------------------------------

# Transform the grid into a SpatialPixels object
grid.centroids <- st_coordinates(st_centroid(grid$x))
grid.points <- SpatialPoints(grid.centroids, proj4string = CRS(st_crs(grid)$proj4string))
grid.pixels <- SpatialPixels(grid.points)

# Estimate Utilization Distribution (UD) across the grid
Tmax <- 3600
Lmin <- 10
hmin <- 5000
maps <- list()
maps.years <- list()
years <- unique(year(gps_data$datetime))
for(y in 1:length(years)){
  gps_data.y <- subset(gps_data, year(gps_data$datetime) == years[y])
  data.traj <- as.ltraj(xy = st_coordinates(gps_data.y), date = gps_data.y$datetime,
                        id = gps_data.y$indID, burst = gps_data.y$indID)
  D <- BRB.D(data.traj, Tmax = Tmax, Lmin = Lmin)
  ud <- BRB(data.traj, D = D, Tmax = Tmax, Lmin = Lmin, hmin = hmin, type = 'UD', grid = grid.pixels)
  id <- unique(gps_data.y$indID)
  for(i in 1:length(id)) {
    maps[[i]] <- raster(ud[[i]])
  }
  weights <- c(rep(1, length(id)))
  maps.mean <- do.call(mosaic, c(maps, fun = weighted.mean, w = weights))
  maps.years[[y]] <- maps.mean
}
weights <- c(rep(1, length(years)))
kerUD <- do.call(mosaic, c(maps.years, fun = weighted.mean, w = weights))
#plot(kerUD)

# Convert RasterLayer to SpatialPixelsDataFrame
kerUD_spdf <- as(kerUD, "SpatialPixelsDataFrame")

# Create an object of class estUD
kerUD_spdf <- new("estUD", kerUD_spdf)

# Assign values to a couple of slots of the estUD object
kerUD_spdf@vol <- FALSE
kerUD_spdf@h$meth <- "Plug-in Bandwidth"

# Convert UD values into volume using getvolumeUD from adehabitatHR and cast over to a raster
estUD <- getvolumeUD(kerUD_spdf, standardize = TRUE)
#plot(estUD)
estUD <- as.data.frame.estUD(estUD)
colnames(estUD) <- c("UD_all", "centroid.x", "centroid.y")
estUD <- estUD %>% arrange(desc(centroid.y))

# Add UD_all to grid cell attributes
grid$UD_all <- estUD$UD_all

# Calculate probability of use from UD
grid$probUse <- (100 - round(grid$UD_all, 2))/100
grid$probUse[grid$cellUse == "land" | grid$cellUse == "colony"] <- 0 # probUse = 0 for land and colony cells

## 4/ Probability of foraging ------------------------------------------------------------------------------

# Estimate Utilization Distribution (UD) across the grid including only "foraging" points
gps_data_foraging <- subset(gps_data, behaviour == "foraging")
maps <- list()
maps.years <- list()
for(y in 1:length(years)){
  gps_data_foraging.y <- subset(gps_data_foraging, year(gps_data_foraging$datetime) == years[y]) %>%
    dplyr::select(indID, geometry)
  id_birds <- unique(gps_data_foraging.y$indID)
  for(i in 1:length(id_birds)){
    gps_data_foraging.y.i <-  subset(gps_data_foraging.y, indID == id_birds[i])
    kerUD_forage.y.i <- kernelUD(as_Spatial(gps_data_foraging.y.i), h = 4000, grid = grid.pixels)
    spixdf <- estUDm2spixdf(kerUD_forage.y.i)
    maps[[i]] <- raster(spixdf)
  }
  weights <- c(rep(1, length(id_birds)))
  kerUD_forage <- do.call(mosaic, c(maps, fun = weighted.mean, w = weights))
  maps.years[[y]] <- kerUD_forage
}
weights <- c(rep(1, length(years)))
kerUD_forage <- do.call(mosaic, c(maps.years, fun = weighted.mean, w = weights))
#plot(kerUD_forage)

# Convert RasterLayer to SpatialPixelsDataFrame
kerUD_forage_spdf <- as(kerUD_forage, "SpatialPixelsDataFrame")

# Create an object of class estUD
kerUD_forage_spdf <- new("estUD", kerUD_forage_spdf)

# Assign values to a couple slots of the estUD
kerUD_forage_spdf@vol <- FALSE
kerUD_forage_spdf@h$meth <- "Plug-in Bandwidth"

# Convert the UD values to volume using getvolumeUD from adehabitatHR and cast over to a raster
estUD_forage <- getvolumeUD(kerUD_forage_spdf, standardize = TRUE)
#plot(estUD_forage)
estUD_forage <- as.data.frame.estUD(estUD_forage)
colnames(estUD_forage) <- c("UD_foraging", "centroid.x", "centroid.y")
estUD_forage <- estUD_forage %>% arrange(desc(centroid.y))

# Add UD_foraging to grid cell attributes
grid$UD_foraging <- estUD_forage$UD_foraging

# Calculate probability of foraging from UD_foraging
grid$probForaging <- (100 - round(grid$UD_foraging, 2))/100
grid$probForaging[grid$cellUse == "land" | grid$cellUse == "colony"] <- 0 # probForaging = zero for land and colony cells

# Save cell attributes in a csv file -----------------------------------------------------------------------
write_csv2(st_drop_geometry(grid), here("data", "cell_attributes.csv"))

# Plot maps ------------------------------------------------------------------------------------------------

library(patchwork)

owf_map <- ggplot() +
  geom_sf(data = coastline, fill = "#D9D9D9", color = "grey50") +
  geom_path(data = gps_data,
            aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], color = indID),
            linewidth = 0.6,
            show.legend = FALSE) +
  geom_sf(data = owf, aes(fill = "OWF"), color = "black", alpha = 0.8) +
  geom_sf(data = colony, shape = 17, size = 2, color = "black") +
  labs(title = "a)") +
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 1) +
  annotation_north_arrow(
    location = "tl", height = unit(1, "cm"), width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering(), which_north = "true",
  ) +
  theme_bw() +
  theme(
    plot.title   = element_text(size = 18),
    legend.position = c(0.01, 0.22),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    legend.text = element_text(size = 14),
    axis.title = element_blank(),
    axis.text = element_text(size = 14),
    plot.margin = margin(5,5,5,5)
  ) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  scale_fill_manual(
    name = "",
    values = c("OWF" = "#E69F00")
  )

grid_map <- ggplot() +
  geom_sf(data = grid, aes(fill = as.factor(cellUse)), color = NA) +
  scale_fill_manual(name = "Cell type",
                    values = c("land" = "#009E73", "sea" = "#56B4E9",
                               "owf" = "#E69F00", "colony" = "#F0E442"),
                    labels = c("Colony", "Land", "OWF", "Sea")) +
  theme_bw() +
  labs(title = "b)") +
  theme(
    legend.position = c(0.87, 0.26),
    plot.title = element_text(size = 18),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    plot.margin = margin(5,5,5,5)) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

library(viridis)

map_probUse <- ggplot() +
  geom_sf(data = coastline, color = "grey30", fill = "grey70") +
  geom_sf(data = grid[grid$probUse > 0,], aes(fill = probUse)) +
  labs(title = "c)", fill = "probUse") +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 10)) +
  geom_sf(data = colony, size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 12, vjust = 0.9),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        plot.margin = margin(5,5,5,5)) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

map_probForaging <- ggplot() +
  geom_sf(data = coastline, color = "grey30", fill = "grey70") +
  geom_sf(data = grid[grid$probUse > 0,], aes(fill = probForaging)) +
  labs(title = "d)", fill = "probForaging") +
  scale_fill_viridis(guide = guide_colorbar(barwidth = 10)) +
  geom_sf(data = colony, size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 12, vjust = 0.9),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        plot.margin = margin(5,5,5,5)) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

common_coord <- coord_sf(
  xlim = c(xmin, xmax),
  ylim = c(ymin, ymax),
  expand = FALSE
)

owf_map <- owf_map + common_coord
grid_map <- grid_map + common_coord
map_probUse <- map_probUse + common_coord
map_probForaging <- map_probForaging + common_coord

merged_maps <- (owf_map | grid_map) /
  (map_probUse | map_probForaging) +
  plot_layout(heights = c(1, 1))

ggsave(here("maps.jpg"), merged_maps,
       width = 30, 
       height = 20, 
       units = "cm")
