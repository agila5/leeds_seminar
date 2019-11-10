# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)
library(stplanr)
library(stats19)
library(spatstat)
library(geofabric)
library(osmdata)
library(ggspatial)
library(igraph)
library(magrittr)
library(units)
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

# slide 6
ggplot() + 
  geom_sf(data = st_boundary(iow_sf_polygon)) +
  geom_sf(data = car_crashes_2018_iow, size = 2) +
  theme_light()
# ggsave("presentation/images/iow_crashes.eps", device = "eps", width = 10, height = 5.5)

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

# quadrat counts ----------------------------------------------------------
iow_bbox <- st_bbox(iow_sf_polygon)
iow_polygon <- st_coordinates(iow_sf_polygon)
iow_owin <- owin(iow_bbox[c(1, 3)], iow_bbox[c(2, 4)],
  poly = list(x = iow_polygon[, 1], y = iow_polygon[, 2])
)
iow_ppp <- ppp(
  x = st_coordinates(car_crashes_2018_iow)[, 1],
  y = st_coordinates(car_crashes_2018_iow)[, 2],
  window = iow_owin
)
iow_density <- density(iow_ppp, diggle = TRUE)
h <- quadratcount(iow_ppp, nx = 10)

# slide 7
par(mar = rep(1.5, 4))
plot(intensity(h, image = TRUE), main = NULL)
points(st_coordinates(iow_main_highways)[,1], st_coordinates(iow_main_highways)[,2],
       cex = 0.01, col = "lightgrey")
points(iow_ppp, pch = 16, cex = 1.6)


# street networks problems ------------------------------------------------

# slide 9
ggplot() +
  geom_sf(data = st_boundary(iow_sf_polygon)) +
  geom_sf(data = iow_main_highways, aes(col = highway, fill = highway), size = 1.05) +
  theme_light() +
  labs(col = "Highway Type", fill = "Highway Type") +
  scale_color_viridis_d() +
  scale_fill_viridis_d()
# ggsave("presentation/images/highway_type.eps", device = "eps", width = 10, height = 5.5)

# download data
westyorkshire <- geofabric::get_geofabric("west yorkshire")
 
# subset data with a 5km buffer centered in ChaptelTown
place_name <- "chapeltown leeds"
place_point <- tmaptools::geocode_OSM(place_name)
place_df <- data.frame(name = place_name, lon = place_point$coords[1], lat = place_point$coords[2])
place_sf <- sf::st_as_sf(place_df, coords = c("lon", "lat"), crs = 4326)
place_buffer <- stplanr::geo_projected(place_sf, sf::st_buffer, dist = 5000)
chapeltown = westyorkshire[place_buffer, ]

key_roads_text = "primary|secondary|tertiary|cycleway|trunk|motorway"
chapeltown_key_roads = chapeltown[grepl(pattern = key_roads_text, x = chapeltown$highway), ] %>%
  st_transform(crs = geo_select_aeq(.))

# roundabouts
roundabout <- chapeltown_key_roads[6, ]
roundabout_and_intersections <- chapeltown_key_roads[roundabout, ]

# slide 13
ggplot() +
  geom_sf(
    data = roundabout_and_intersections,
    size = 1.5,
    col = c("grey", hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:8])
  ) +
  geom_sf(
    data = line2points(roundabout_and_intersections),
    col = rep(c("black", hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:8]), each = 2),
    size = 3
  ) +
  theme_light()
# ggsave("presentation/images/roundabout1.eps", device = "eps", width = 5.5, height = 10)

set.seed(1)
roundabout_clean <- stplanr::rnet_breakup_vertices(roundabout_and_intersections) %>%
  mutate(plot_ID = as.character(sample(1:n())))

# slide 13
ggplot() +
  geom_sf(
    data = roundabout_clean,
    size = 1.5,
    mapping = aes(col = plot_ID, fill = plot_ID),
    show.legend = FALSE
  ) +
  theme_light()
# ggsave("presentation/images/roundabout2.eps", device = "eps", width = 5.5, height = 10)

# overpasses
overpass_buffer <- st_as_sf(
  data.frame(lon = -1.554954, lat = 53.799695),
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  st_transform(st_crs(chapeltown_key_roads)) %>%
  st_buffer(25)
overpass_system <- chapeltown_key_roads[overpass_buffer, ]
overpass_clean <- rnet_breakup_vertices(overpass_system) %>%
  mutate(ID_split = as.character(1:n()))

# slide 14
ggplot() +
  geom_sf(
    data = overpass_system,
    size = 1.5,
    col = hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:8]
  ) +
  geom_sf(
    data = line2points(overpass_system),
    size = 3
  ) +
  theme_light()
# ggsave("presentation/images/overpasses1.eps", device = "eps", width = 10, height = 5.5)

# slide 14
ggplot() +
  geom_sf(
    data = overpass_clean,
    size = 1.5,
    col = hcl(h = seq(15, 375, length = 9), l = 65, c = 100)[1:9]
  ) +
  geom_sf(
    data = line2points(overpass_system),
    size = 3
  ) +
  theme_light()
# ggsave("presentation/images/overpasses2.eps", device = "eps", width = 10, height = 5.5)

# streets intersections
intersections_buffer <- st_as_sf(
  data.frame(lon = -1.545201, lat = 53.814019),
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  st_transform(st_crs(chapeltown_key_roads)) %>%
  st_buffer(dist = 15)
intersections_system <- chapeltown_key_roads[intersections_buffer, ]

# slide 15
ggplot() +
  geom_sf(
    data = intersections_system,
    mapping = aes(col = osm_id, fill = osm_id),
    size = 1.25,
    show.legend = FALSE
  ) +
  geom_sf(
    data = line2points(intersections_system),
    size = 2.5
  ) +
  theme_light()
ggsave("presentation/images/intersections1.eps", device = "eps", width = 10, height = 5.5)


intersections_system_clean <- rnet_breakup_vertices(intersections_system) %>%
  mutate(plot_ID = as.character(1:n()))

# slide 15
ggplot() +
  geom_sf(
    data = intersections_system_clean,
    mapping = aes(col = plot_ID, fill = plot_ID),
    size = 1.25,
    show.legend = FALSE
  ) +
  geom_sf(
    data = line2points(intersections_system_clean),
    size = 2.5
  ) +
  theme_light()
# ggsave("presentation/images/intersections2.eps", device = "eps", width = 10, height = 5.5)


# Fixing the network ------------------------------------------------------
iow_main_highways_breakup <- rnet_breakup_vertices(iow_main_highways)

# slide 16
ggplot() +
  geom_sf(data = st_boundary(iow_sf_polygon)) +
  geom_sf(data = iow_main_highways_breakup, aes(col = highway, fill = highway), show.legend = FALSE, size = 1.25) +
  geom_sf(data = car_crashes_2018_iow, size = 1.75) +
  theme_light() +
  scale_fill_viridis_d() +
  scale_color_viridis_d()
# ggsave("presentation/images/breaking_network.eps", device = "eps", width = 10, height = 5.5)

# nearest street ----------------------------------------------------------
car_crashes_2018_iow <- car_crashes_2018_iow[
  iow_main_highways_breakup, 
  op = st_is_within_distance, 
  dist = units::set_units(100, "m")
]
iow_main_highways_breakup$number_of_car_crashes <- st_nearest_feature(car_crashes_2018_iow, iow_main_highways_breakup) %>% 
  factor(levels = seq_len(nrow(iow_main_highways_breakup))) %>% table() %>% as.numeric()
iow_main_highways_breakup$segment_length <- st_length(iow_main_highways_breakup)

# slide 18
iow_main_highways_breakup %>%
  mutate(number_of_car_crashes = as.character(number_of_car_crashes)) %>%
  ggplot() +
  geom_sf(aes(col = number_of_car_crashes, fill = number_of_car_crashes), size = 1.25) +
  scale_color_brewer(palette = "RdYlGn", direction = -1, guide = guide_legend(nrow = 1, ncol = 6)) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, guide = guide_legend(nrow = 1, ncol = 6)) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(col = "", fill = "")
# ggsave("presentation/images/count_on_nearest_street.eps", device = "eps", width = 10, height = 5.5)

iow_main_highways_breakup <- iow_main_highways_breakup %>% 
  mutate(number_of_car_crashes_per_meter =as.numeric(number_of_car_crashes / st_length(.)))

# slide 20
ggplot(iow_main_highways_breakup) +
  geom_sf(aes(col = number_of_car_crashes_per_meter, fill = number_of_car_crashes_per_meter), size = 1.25) +
  scale_color_distiller(palette = "RdYlGn", direction = -1) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  theme_light() +
  labs(col = "", fill = "")
# ggsave("presentation/images/car_crashes_per_meter.eps", device = "eps", width = 10, height = 5.5)

# smoothing ---------------------------------------------------------------
iow_main_highways_breakup_graph <- st_touches(iow_main_highways_breakup) %>% graph.adjlist()
iow_main_highways_breakup_graph_ego <- ego(iow_main_highways_breakup_graph, order = 2)

spatial_smoothing <- function(ID, var, graph_ego) {
  mean(as.numeric({{var}}[graph_ego[[ID]]]))
}

iow_main_highways_breakup <- iow_main_highways_breakup %>%
  mutate(
    number_of_car_crashes_per_meter_smooth = map_dbl(
      seq_len(nrow(.)),
      spatial_smoothing,
      var = number_of_car_crashes_per_meter,
      graph_ego = iow_main_highways_breakup_graph_ego
    )
  )

# slide 23
ggplot(iow_main_highways_breakup) +
  geom_sf(aes(col = number_of_car_crashes_per_meter_smooth, fill = number_of_car_crashes_per_meter_smooth), size = 1.25) +
  scale_color_distiller(palette = "RdYlGn", direction = -1) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  theme_light() +
  labs(col = "", fill = "")
# ggsave("presentation/images/car_crashes_per_meter_smooth6.eps", device = "eps", width = 10, height = 5.5)

iow_main_highways_graph <- st_relate(iow_main_highways, pattern = "****0****") %>%
  graph.adjlist()
iow_main_highways_graph_ego <- ego(iow_main_highways_graph, order = 1405)

# slide 24
iow_main_highways %>%
  mutate(
    number_of_car_crashes = st_nearest_feature(car_crashes_2018_iow, iow_main_highways) %>%
           factor(levels = seq_len(nrow(iow_main_highways))) %>% table() %>% as.numeric(),
    number_of_car_crashes_per_meter = as.numeric(number_of_car_crashes / st_length(iow_main_highways)),
    number_of_car_crashes_per_meter_smooth = map_dbl(
      seq_len(nrow(.)),
      spatial_smoothing,
      var = number_of_car_crashes_per_meter,
      graph_ego = iow_main_highways_graph_ego
    )
  ) %>%
  ggplot() +
  geom_sf(aes(col = number_of_car_crashes_per_meter_smooth, fill = number_of_car_crashes_per_meter_smooth), size = 1.25, show.legend = FALSE) +
  scale_color_distiller(palette = "RdYlGn", direction = -1) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  theme_light()
# ggsave("presentation/images/importance_of_network_cleaning.eps", device = "eps", width = 10, height = 5.5)


# empirical bayes ---------------------------------------------------------
iow_main_highways_breakup_graph <- st_touches(iow_main_highways_breakup) %>% graph.adjlist()
empirical_bayes <- function(ID, x, l, graph_ego) {
  # raw ratio
  y <- x[ID] / l[ID]
  
  # neighborhood
  delta <- as.vector(graph_ego[[ID]])
  
  # empirical version of the mean of the prior
  x <- x[delta]
  l <- l[delta]
  mu_tilde <- sum(x) / sum(l)
  
  # empirical version of the variance of the prior
  s2 <- sum((x - l * mu_tilde) ^ 2 / l) / sum(l)
  sigma2_tilde <- max(0, s2 - mu_tilde / mean(l))
  
  # empirical version of Pi
  P <- ifelse(sigma2_tilde + mu_tilde / mean(l) == 0, 1, sigma2_tilde / (sigma2_tilde + mu_tilde / mean(l)))
  
  # result
  theta <- P * y + (1 - P) * mu_tilde
  theta
}

iow_main_highways_breakup_graph_ego <- ego(iow_main_highways_breakup_graph, order = 6)

iow_main_highways_breakup <- iow_main_highways_breakup %>%
  mutate(
    empirical_bayes_estimate = map_dbl(
      seq_len(nrow(.)),
      empirical_bayes,
      x = number_of_car_crashes, 
      l = as.numeric(st_length(.)), 
      graph_ego = iow_main_highways_breakup_graph_ego
    )
  )

# slide 34
ggplot(iow_main_highways_breakup) +
  geom_sf(aes(col = empirical_bayes_estimate, fill = empirical_bayes_estimate), size = 1.25) +
  scale_color_distiller(palette = "RdYlGn", direction = -1) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  theme_light() +
  labs(col = "", fill = "")
# ggsave("presentation/images/empirical_bayes_1.eps", device = "eps", width = 10, height = 5.5)
# it doesn't work whatever is the order of the neighborhood. Why?

iow_main_highways_breakup %>% 
  select(segment_length, empirical_bayes_estimate) %>% 
  st_drop_geometry() %>% 
  arrange(desc(empirical_bayes_estimate))

ggplot(iow_main_highways_breakup) + 
  geom_histogram(aes(x = as.numeric(segment_length)), bins = 40) + 
  scale_x_continuous(trans = "log")
# let's try cutting up the network, I'd like something uniform

# define the threshold
threshold <- quantile(as.numeric(iow_main_highways_breakup$segment_length), probs = c(0.15)) %>% 
  units::set_units("m")
threshold

# extract the geometry
iow_main_highways_breakup_geometry <- st_geometry(iow_main_highways_breakup)

# sort it according to length
iow_main_highways_breakup_geometry <- iow_main_highways_breakup_geometry[order(st_length(iow_main_highways_breakup_geometry))]

# define the neighborhoods
my_neigh <- st_touches(iow_main_highways_breakup_geometry) %>% graph.adjlist() %>% ego(1)

# let's start a loop
any_segment_too_short <- any(st_length(iow_main_highways_breakup_geometry) < threshold)

while(any_segment_too_short) {
  for(i in seq_len(length(iow_main_highways_breakup_geometry))) {
    if(st_length(iow_main_highways_breakup_geometry[i]) < threshold) {
      # i is index of the "too short" segment
      # look for the indexes of its neighbours
      index_neighbours <- as.vector(my_neigh[[i]])[-1]
      
      # look for shortest neighbours
      index_shortest_neighbours <- which.min(st_length(iow_main_highways_breakup_geometry[index_neighbours]))
      
      # extract the short segment
      short_segment <- iow_main_highways_breakup_geometry[i]
      short_segment_neighbour <- iow_main_highways_breakup_geometry[as.vector(my_neigh[[i]])[index_shortest_neighbours + 1]]
      
      # exclude them from original linestring
      iow_main_highways_breakup_geometry <- iow_main_highways_breakup_geometry[-c(i, as.vector(my_neigh[[i]])[index_shortest_neighbours + 1])]
      
      # merge them
      merging_segments <- st_union(short_segment, short_segment_neighbour) %>% st_line_merge()
      
      # append them to the structure
      iow_main_highways_breakup_geometry <- c(iow_main_highways_breakup_geometry, merging_segments)
      
      # sort the sfc according to length
      iow_main_highways_breakup_geometry <- iow_main_highways_breakup_geometry[order(st_length(iow_main_highways_breakup_geometry))]
      
      # rebuild the graph structure
      my_neigh <- st_touches(iow_main_highways_breakup_geometry) %>% graph.adjlist() %>% ego(1)
      
      break
    }
  }
  print(sum(st_length(iow_main_highways_breakup_geometry) < threshold))
  any_segment_too_short <- any(st_length(iow_main_highways_breakup_geometry) < threshold)
}

# rebuild the sf
my_sf <- st_sf(
  tibble(segment_length = st_length(iow_main_highways_breakup_geometry)), 
  geometry = iow_main_highways_breakup_geometry
)

# calculate number of car crashes per road segment
my_sf$number_of_car_crashes <- st_nearest_feature(car_crashes_2018_iow, my_sf) %>% 
  factor(levels = seq_len(nrow(my_sf))) %>% table() %>% as.numeric()

# rebuild the graph structure
my_graph <- st_touches(my_sf) %>% graph.adjlist() %>% ego(1)

my_sf <- my_sf %>%
  mutate(
    empirical_bayes_estimate = map_dbl(
      seq_len(nrow(.)),
      empirical_bayes,
      x = number_of_car_crashes, 
      l = as.numeric(segment_length), 
      graph_ego = my_graph
    )
  )

# slide 38
ggplot(my_sf) + 
  geom_sf(data = st_boundary(iow_sf_polygon)) + 
  geom_sf(aes(col = empirical_bayes_estimate, fill = empirical_bayes_estimate), size = 1.25) +
  scale_color_distiller(palette = "RdYlGn", direction = -1, trans = "sqrt") +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, trans = "sqrt") +
  theme_light() +
  labs(col = "", fill = "")

ggplot(my_sf) + 
  geom_sf(data = st_boundary(iow_sf_polygon)) + 
  geom_sf(aes(col = empirical_bayes_estimate ^ (0.33), fill = empirical_bayes_estimate ^ (0.33)), size = 1.25) +
  scale_color_distiller(palette = "RdYlGn", direction = -1) +
  scale_fill_distiller(palette = "RdYlGn", direction = -1) +
  theme_light() +
  labs(col = "", fill = "")
