# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(stplanr)
library(stats19)
library(spatstat)
library(geofabric)
library(osmdata)
library(ggspatial)
library(conflicted)

# resolve conflicts
conflict_prefer("filter", "dplyr")

# load data ---------------------------------------------------------------

# download sf polygon for IOW (for filters and so on)
# iow_sf_polygon <- getbb("Isle of Wight, South East, England", format_out = "sf_polygon", featuretype = "state") %>%
#   st_transform(crs = 27700)
# st_write(iow_sf_polygon, "data/iow_sf_polygon.gpkg")
iow_sf_polygon <- read_sf("data/iow_sf_polygon.gpkg")

# download car crashes data for 2018
# car_crashes_2018 <- get_stats19("2018") %>% 
#   format_sf()
# filter car crashes occurred in IOW
# car_crashes_2018_iow <- car_crashes_2018[iow_sf_polygon, ]
# st_write(car_crashes_2018_iow, "data/car_crashes_2018_iow.gpkg")
car_crashes_2018_iow <- read_sf("data/car_crashes_2018_iow.gpkg")

# plot
# p <- ggplot() + 
#   geom_sf(data = st_boundary(iow_sf_polygon)) + 
#   geom_sf(data = car_crashes_2018_iow, size = 2) + 
#   theme_light()
# p
# ggsave("presentation/images/iow_crashes.eps", plot = p, device = "eps", width = 10, height = 5.5)

# download network data 
# iow_highways <- get_geofabric("Isle of Wight") %>% st_transform(27700)
# filter only main roads
# key_roads_text <- "primary|secondary|tertiary"
# iow_main_highways <- iow_highways %>%
#   filter(grepl(pattern = key_roads_text, x = highway)) %>%
#   mutate(highway = sub("_link", "", highway)) %>%
#   mutate(highway = str_to_title(highway))
# st_write(iow_main_highways, "data/iow_main_highways.gpkg")
iow_main_highways <- read_sf("data/iow_main_highways.gpkg") %>% rename(geometry = geom)
#

# quadrat counts ----------------------------------------------------------

# iow_bbox <- st_bbox(iow_sf_polygon)
# iow_polygon <- st_coordinates(iow_sf_polygon)
# iow_owin <- owin(iow_bbox[c(1, 3)], iow_bbox[c(2, 4)],
#   poly = list(x = iow_polygon[, 1], y = iow_polygon[, 2])
# )
# iow_ppp <- ppp(
#   x = st_coordinates(car_crashes_2018_iow)[, 1], 
#   y = st_coordinates(car_crashes_2018_iow)[, 2], 
#   window = iow_owin
# )
# iow_density <- density(iow_ppp, diggle = TRUE)
# h <- quadratcount(iow_ppp, nx = 10)

# # plot raw quadrat counts
# png(filename = "presentation/images/quadratcount.png", width = 960, height = 528, units = "px")
# par(mar = rep(1.5, 4))
# plot(intensity(h, image = TRUE), main = NULL)
# points(st_coordinates(iow_main_highways)[,1], st_coordinates(iow_main_highways)[,2], 
#        cex = 0.01, col = "lightgrey")
# points(iow_ppp, pch = 16, cex = 1.6)
# dev.off()
# dev.off()
# 
# setEPS()
# postscript(file = "presentation/images/quadratcount2.eps", width = 10, height = 5.5, horizontal = FALSE)
# par(mar = rep(1.5, 4))
# plot(intensity(h, image = TRUE), main = NULL)
# points(st_coordinates(iow_main_highways)[,1], st_coordinates(iow_main_highways)[,2], 
#        cex = 0.01, col = "lightgrey")
# points(iow_ppp, pch = 16, cex = 1.6)
# dev.off()
# dev.off()

# street networks problems ------------------------------------------------

# iow network plot
# p <- ggplot() + 
#   geom_sf(data = st_boundary(iow_sf_polygon)) + 
#   geom_sf(data = iow_main_highways, aes(col = highway, fill = highway), size = 1.05) + 
#   theme_light() + 
#   labs(col = "Highway Type", fill = "Highway Type") + 
#   scale_color_viridis_d() + 
#   scale_fill_viridis_d()
# p
# ggsave("presentation/images/highway_type.eps", plot = p, device = "eps", width = 10, height = 5.5)


# # download data
# westyorkshire <- geofabric::get_geofabric("west yorkshire")
# 
# # subset data with a 5km buffer centered in ChaptelTown
# place_name <- "chapeltown leeds"
# place_point <- tmaptools::geocode_OSM(place_name)
# place_df <- data.frame(name = place_name, lon = place_point$coords[1], lat = place_point$coords[2])
# place_sf <- sf::st_as_sf(place_df, coords = c("lon", "lat"), crs = 4326)
# place_buffer <- stplanr::geo_projected(place_sf, sf::st_buffer, dist = 5000)
# chapeltown = westyorkshire[place_buffer, ]
# # eventually we will consider (mostly) every highway type
# key_roads_text = "primary|secondary|tertiary|cycleway|trunk|motorway"
# chapeltown_key_roads = chapeltown[grepl(pattern = key_roads_text, x = chapeltown$highway), ] %>% 
#   st_transform(crs = geo_select_aeq(.))
# 
# # roundabouts
# roundabout <- chapeltown_key_roads[6, ]
# roundabout_and_intersections <- chapeltown_key_roads[roundabout, ]
# 
# p <- ggplot() + 
#   geom_sf(
#     data = roundabout_and_intersections, 
#     size = 1.5, 
#     col = c("grey", hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:8])
#   ) + 
#   geom_sf(
#     data = line2points(roundabout_and_intersections), 
#     col = rep(c("black", hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:8]), each = 2),
#     size = 3
#   ) + 
#   theme_light()
# p
# ggsave("presentation/images/roundabout1.eps", plot = p, device = "eps", width = 5.5, height = 10)
# 
# set.seed(1)
# roundabout_clean <- stplanr::rnet_breakup_vertices(roundabout_and_intersections) %>% 
#   mutate(plot_ID = as.character(sample(1:n())))
# 
# p <- ggplot() + 
#   geom_sf(
#     data = roundabout_clean, 
#     size = 1.5, 
#     mapping = aes(col = plot_ID, fill = plot_ID), 
#     show.legend = FALSE
#   ) + 
#   theme_light()
# p
# ggsave("presentation/images/roundabout2.eps", plot = p, device = "eps", width = 5.5, height = 10)
# 
# # overpasses
# overpass_buffer <- st_as_sf(
#   data.frame(lon = -1.554954, lat = 53.799695),
#   coords = c("lon", "lat"),
#   crs = 4326
# ) %>% 
#   st_transform(st_crs(chapeltown_key_roads)) %>% 
#   st_buffer(25)
# overpass_system <- chapeltown_key_roads[overpass_buffer, ]
# overpass_clean <- rnet_breakup_vertices(overpass_system) %>% 
#   mutate(ID_split = as.character(1:n()))
# 
# p <- ggplot() + 
#   geom_sf(
#     data = overpass_system, 
#     size = 1.5, 
#     col = hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:8]
#   ) + 
#   geom_sf(
#     data = line2points(overpass_system), 
#     size = 3
#   ) + 
#   theme_light()
# p
# ggsave("presentation/images/overpasses1.eps", plot = p, device = "eps", width = 10, height = 5.5)
# 
# p <- ggplot() + 
#   geom_sf(
#     data = overpass_clean, 
#     size = 1.5, 
#     col = hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:9]
#   ) + 
#   geom_sf(
#     data = line2points(overpass_system), 
#     size = 3
#   ) + 
#   theme_light()
# p
# ggsave("presentation/images/overpasses2.eps", plot = p, device = "eps", width = 10, height = 5.5)
# 
# 
# # streets intersections
# intersections_buffer <- st_as_sf(
#   data.frame(lon = -1.545201, lat = 53.814019),
#   coords = c("lon", "lat"),
#   crs = 4326
# ) %>% 
#   st_transform(st_crs(chapeltown_key_roads)) %>% 
#   st_buffer(dist = 15)
# intersections_system <- chapeltown_key_roads[intersections_buffer, ]
# 
# p <- ggplot() + 
#   geom_sf(
#     data = intersections_system,
#     mapping = aes(col = osm_id, fill = osm_id), 
#     size = 1.25, 
#     show.legend = FALSE
#   ) + 
#   geom_sf(
#     data = line2points(intersections_system), 
#     size = 2.5
#   ) + 
#   theme_light()
# p
# ggsave("presentation/images/intersections1.eps", plot = p, device = "eps", width = 10, height = 5.5)
# 
# intersections_system_clean <- rnet_breakup_vertices(intersections_system) %>% 
#   mutate(plot_ID = as.character(1:n()))
# 
# p <- ggplot() + 
#   geom_sf(
#     data = intersections_system_clean,
#     mapping = aes(col = plot_ID, fill = plot_ID), 
#     size = 1.25, 
#     show.legend = FALSE
#   ) + 
#   geom_sf(
#     data = line2points(intersections_system_clean), 
#     size = 2.5
#   ) + 
#   theme_light()
# p
# ggsave("presentation/images/intersections2.eps", plot = p, device = "eps", width = 10, height = 5.5)


# Fixing the network ------------------------------------------------------

iow_main_highways_breakup <- rnet_breakup_vertices(iow_main_highways)
# p <- ggplot() + 
#   geom_sf(data = st_boundary(iow_sf_polygon)) + 
#   geom_sf(data = iow_main_highways_breakup, aes(col = highway, fill = highway), show.legend = FALSE, size = 1.25) + 
#   geom_sf(data = car_crashes_2018_iow, size = 1.75) + 
#   theme_light() + 
#   scale_fill_viridis_d() + 
#   scale_color_viridis_d()
# p
# ggsave("presentation/images/breaking_network.eps", plot = p, device = "eps", width = 10, height = 5.5)



# nearest street ----------------------------------------------------------
car_crashes_2018_iow <- car_crashes_2018_iow[
  iow_main_highways_breakup, 
  op = st_is_within_distance, 
  dist = units::set_units(100, "m")
]
iow_main_highways_breakup$number_of_car_crashes <- st_nearest_feature(car_crashes_2018_iow, iow_main_highways_breakup) %>% 
  factor(levels = seq_len(nrow(iow_main_highways_breakup))) %>% table() %>% as.numeric()

p <- iow_main_highways_breakup %>% 
  mutate(number_of_car_crashes = as.character(number_of_car_crashes)) %>% 
  ggplot() + 
  geom_sf(aes(col = number_of_car_crashes, fill = number_of_car_crashes), size = 1.25) +  
  scale_color_brewer(palette = "RdYlGn", direction = -1) + 
  scale_fill_brewer(palette = "RdYlGn", direction = -1) + 
  theme_light() + 
  theme(legend.position = "bottom", legend.box = "horizontal") + 
  labs(col = "", fill = "")
p
# smoothing ---------------------------------------------------------------


