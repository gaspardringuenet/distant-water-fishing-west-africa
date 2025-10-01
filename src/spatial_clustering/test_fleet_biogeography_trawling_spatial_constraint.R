library(vegan)
library(dplyr)
library(tidyr)
library(Matrix)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scales)
library(betapart)
library(cluster)

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

# Step 1: Filter to shelf trawling effort ---------------------------------
effort_shelf_trawl <- effort %>%
  filter(gear_type == "Trawlers" & distance_to_shore_km <= 200)

# Step 2: Build site x fleet-group matrix ----------------------------------
site_flag_df <- effort_shelf_trawl %>%
  group_by(lat, lon, flag) %>%
  summarise(effort = sum(kWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(cell_id = paste(lat, lon, sep = "_")) |> 
  select(-c(lat, lon))

flag_matrix <- site_flag_df %>%
  pivot_wider(names_from = flag, values_from = effort, values_fill = 0) %>%
  column_to_rownames("cell_id")


library(ClustGeo)
library(geosphere)

# ----------------------------
# 1. Input data: fleet matrix
# ----------------------------
# You should already have this:
# `mmsi_matrix`: rows = grid cells, cols = MMSI or fleet group
# Make sure rows are filtered for non-zero effort
# Also keep track of lat/lon of each row:

row_sums <- rowSums(flag_matrix)
empty_rows <- which(row_sums == 0)
flag_matrix <- flag_matrix[row_sums > 0, ]

cell_coords <- rownames(flag_matrix) %>%
  strsplit("_") %>%
  lapply(as.numeric) %>%
  do.call(rbind, .) %>%
  as.data.frame()
colnames(cell_coords) <- c("lat", "lon")

# ----------------------------
# 2. Dissimilarity matrices
# ----------------------------

D0_bray <- vegan::vegdist(as.matrix(flag_matrix_clean), method = "bray")

hellinger_mat <- decostand(as.matrix(flag_matrix), method = "hellinger")
D0_hell <- dist(hellinger_mat, method = "euclidean")

# B. Geographic distances (in km)
coords_mat <- as.matrix(cell_coords[, c("lon", "lat")])
D1 <- distm(coords_mat) / 1000  # meters to km
D1 <- as.dist(D1)  # Convert to 'dist' object

# ----------------------------
# 3. Spatially-constrained clustering
# ----------------------------


# silhouette_grid_search: run silhouette over alpha and k ranges



tree <- hclustgeo(D0, D1, alpha = 0.5)

# Cut tree
k <- 9 # Try different values!
cluster_groups <- cutree(tree, k)

# Add cluster IDs to coordinates
fleet_clusters <- cbind(cell_coords, cluster = cluster_groups)

ggplot(fleet_clusters, aes(x = lon, y = lat, fill = as.factor(cluster))) +
  geom_tile() +
  coord_fixed() +
  scale_fill_brewer(palette = "Set3", name = "Cluster") +
  theme_minimal() +
  labs(title = "Spatially-Constrained Fleet Composition Clusters")
