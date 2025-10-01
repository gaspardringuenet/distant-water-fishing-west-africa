library(vegan)
library(dplyr)
library(tidyr)
library(Matrix)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scales)
library(betapart)

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

# Step 2: Aggregate effort to 0.5° cells -----------------------------------
effort_binned <- effort_shelf_trawl %>%
  mutate(
    lat = round(lat * 2) / 2,
    lon = round(lon * 2) / 2,
  )

effort_binned <- effort_shelf_trawl

# Step 3: Build site x fleet-group matrix ----------------------------------
site_fg_df <- effort_binned %>%
  group_by(lat, lon, flag) %>%
  summarise(effort = sum(kWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(cell_id = paste(lat, lon, sep = "_")) |> 
  select(-c(lat, lon))

fg_matrix <- site_fg_df %>%
  pivot_wider(names_from = flag, values_from = effort, values_fill = 0) %>%
  column_to_rownames("cell_id")

# Step 4: Transform data with Hellinger ------------------------------------
fg_hellinger <- decostand(fg_matrix, method = "hellinger")

# Step 5: Distance matrix + clustering -------------------------------------
fg_dist <- dist(fg_hellinger, method = "euclidean")
hc <- hclust(fg_dist, method = "ward.D2")

# Elbow method to determine optimal number of clusters
wss <- numeric(30)  # Vector to store WSS for k = 1 to 15

for (k in 1:30) {
  # Cut tree into k clusters
  clusters_k <- cutree(hclust(fg_dist, method = "ward.D2"), k = k)
  
  # Compute WSS for these clusters
  wss_k <- 0
  for (cluster_num in unique(clusters_k)) {
    # Get indices of points in the cluster
    inds <- which(clusters_k == cluster_num)
    # Subset rows of fg_hellinger (your Hellinger data matrix) for this cluster
    cluster_data <- fg_hellinger[inds, , drop = FALSE]
    # Calculate centroid of cluster
    centroid <- colMeans(cluster_data)
    # Sum squared distances to centroid
    wss_k <- wss_k + sum(rowSums((cluster_data - centroid)^2))
  }
  
  wss[k] <- wss_k
}

# Plot WSS versus number of clusters
plot(1:30, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Within-cluster sum of squares (WSS)",
     main = "Elbow method for optimal K")

# Cut into clusters (can try different k)
k <- 8
clusters <- cutree(hc, k = k)

# Step 6: Add cluster info to coordinates ----------------------------------
coords <- rownames(fg_matrix) %>%
  strsplit("_") %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  setNames(c("lat", "lon")) %>%
  mutate(cluster = clusters)

coords$lat <- as.numeric(as.character(coords$lat))
coords$lon <- as.numeric(as.character(coords$lon))

# Step 7: Plot the clustered map -------------------------------------------
bbox = st_bbox(fao34_sf)

ggplot() +
  geom_tile(data = coords, aes(x = lon, y = lat, fill = factor(cluster))) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  scale_fill_brewer(palette = "Set3", name = "Fishing Region") +
  theme_minimal() +
  labs(title = "Fleet-Based Fishing Regions on the Continental Shelf (Trawlers)")

# Step 8: Characterize region composition ----------------------------------
site_fg_df <- site_fg_df %>%
  left_join(coords %>% mutate(cell_id = paste(lat, lon, sep = "_")), by = "cell_id")

summary_df <- site_fg_df %>%
  group_by(cluster, flag) %>%
  summarise(total_effort = sum(effort), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(prop = total_effort / sum(total_effort)) %>%
  arrange(cluster, desc(prop))

summary_df %>%
  ggplot(aes(x = flag, y = prop)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ cluster, scales = "free_y")

summary_df %>%
  ggplot(aes(x = flag, y = total_effort)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ cluster, scales = "free_y")

# Step 9: Optional NMDS for gradients --------------------------------------
nmds <- metaMDS(fg_hellinger, distance = "euclidean", k = 2, trymax = 100)
scores_sites <- scores(nmds, display = "sites")
scores_df <- as.data.frame(scores_sites) %>%
  mutate(cell_id = rownames(fg_matrix)) %>%
  separate(cell_id, into = c("lat", "lon"), sep = "_", convert = TRUE)

# Map NMDS1 axis
scores_df$NMDS1_scaled <- rescale(scores_df$NMDS1)

ggplot() +
  geom_tile(data = scores_df, aes(x = lon, y = lat, fill = NMDS1_scaled)) +
  scale_fill_viridis_c(name = "Trawler Gradient") +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal() +
  labs(title = "Gradient of Trawler Community Composition (NMDS1)")

# Step 10: Optional beta-diversity partition -------------------------------
pa_matrix <- decostand(fg_matrix, method = "pa")
beta_parts <- beta.multi(pa_matrix, index.family = "jaccard")
print(beta_parts)

