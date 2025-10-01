library(vegan)
library(dplyr)
library(tidyr)
library(Matrix)
library(ggplot2)
library(sf)
library(rnaturalearth)

source("src/0-utils/load_all_datasets.R")
source("src/0-utils/gfwr_map_utils.R")
source("src/0-utils/clean_effort.R")

# Clean the apparent fishing effort dataset
min_dist <- 5
min_hours <- 2000

l <- clean_effort(effort, min_dist, min_hours)

total_initial_effort <- l[[1]]
discarded_effort <- l[[2]]
effort <- l[[3]]

effort_binned <- effort %>%
  mutate(
    lat = round(lat * 2) / 2,
    lon = round(lon * 2) / 2,
    flag_gear = paste(flag, gear_type, sep = "_")
  )

site_fg_df <- effort_binned %>%
  group_by(lat, lon, flag_gear) %>%
  summarise(effort = sum(apparent_fishing_hours), .groups = "drop") %>%
  mutate(cell_id = paste(lat, lon, sep = "_")) |> 
  select(-c(lat, lon))

fg_matrix <- site_fg_df %>%
  pivot_wider(names_from = flag_gear, values_from = effort, values_fill = 0) %>%
  column_to_rownames("cell_id")

# Hellinger preferred to reduce dominance bias
fg_hellinger <- decostand(fg_matrix, method = "hellinger")
fg_dist <- dist(fg_hellinger, method = "euclidean")


hc <- hclust(fg_dist, method = "ward.D2")  # good for continuous data
clusters <- cutree(hc, k = 6)  # try various values

coords <- rownames(fg_matrix) %>%
  strsplit("_") %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  setNames(c("lat", "lon")) %>%
  mutate(cluster = clusters)

coords$lat <- as.numeric(as.character(coords$lat))
coords$lon <- as.numeric(as.character(coords$lon))

bbox = st_bbox(fao34_sf)

ggplot() +
  geom_tile(data = coords, aes(x = lon, y = lat, fill = factor(cluster))) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  scale_fill_brewer(palette = "Set3", name = "Fishing Region") +
  theme_minimal() +
  labs(title = "Clustered Fishing Regions Based on Actor Composition")

site_fg_df <- site_fg_df %>%
  left_join(coords %>% mutate(cell_id = paste(lat, lon, sep = "_")), by = "cell_id")

summary_df <- site_fg_df %>%
  group_by(cluster, flag_gear) %>%
  summarise(total_effort = sum(effort), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(prop = total_effort / sum(total_effort)) %>%
  arrange(cluster, desc(prop))
