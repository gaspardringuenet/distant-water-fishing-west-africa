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

# Analysis ----------------------------------------------------------------

# Create new lat/lon bins: round to nearest 0.5
effort_binned <- effort %>%
  mutate(
    lat = round(lat * 2) / 2,
    lon = round(lon * 2) / 2
  ) |> 
  group_by(lat, lon) |> 
  mutate(
    apparent_fishing_hours = sum(apparent_fishing_hours)
  ) |> 
  select(-c(distance_to_shore_km, distance_from_port_km)) |> 
  distinct()

## Build the Site × Species Matrix ----------------------------------------

# Summarize effort by grid cell and vessel
site_species_df <- effort_binned %>%
  group_by(lat, lon, mmsi) %>%
  summarise(effort = sum(apparent_fishing_hours), .groups = "drop")

# Create unique cell ID
site_species_df <- site_species_df %>%
  mutate(cell_id = paste(lat, lon, sep = "_"))

# Step 2: Convert to sparse matrix
site_species_matrix <- site_species_df %>%
  pivot_wider(names_from = mmsi, values_from = effort, values_fill = 0) %>%
  column_to_rownames("cell_id")

# Convert to sparse matrix
mmsi_matrix <- as(as.matrix(site_species_matrix), "dgCMatrix")

## Compute Dissimilarity Matrix -------------------------------------------

# Only keep rows with positive total effort
non_empty <- rowSums(mmsi_matrix) > 0
mmsi_matrix <- mmsi_matrix[non_empty, ]

# Bray-Curtis distances
bc_dist <- vegdist(mmsi_matrix, method = "bray")

# Perform hierarchical clustering using Ward's method (good for Bray-Curtis)
hc <- hclust(bc_dist, method = "ward.D2")

# Cut tree into 5 clusters
k <- 7
cluster_assignments <- as.data.frame(cutree(hc, k = k)) |> 
  rownames_to_column("cell_id") |>
  separate(cell_id, into = c("lat", "lon"), sep = "_", convert = TRUE) |> 
  rename(cluster = "cutree(hc, k = k)")

# Join effort with clusters
effort_binned <- effort_binned |> 
  left_join(cluster_assignments,
            by = c("lat", "lon"))


bbox <- st_bbox(fao34_sf)

effort_binned |> 
  distinct(lat, lon, cluster) |>
  ggplot() +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = as.factor(cluster))) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  #geom_sf(data = eez_overlap_sf, color = "black", fill = NA, size = 0.2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))




# Elbow method ------------------------------------------------------------

# Number of clusters to evaluate
k_range <- 2:20

# Store total within-cluster sum of dissimilarities
wss <- numeric(length(k_range))

# Loop over cluster numbers
for (i in seq_along(k_range)) {
  k <- k_range[i]
  clusters <- cutree(hc, k = k)
  
  # Compute mean within-cluster dissimilarity
  total <- 0
  for (cl in unique(clusters)) {
    members <- which(clusters == cl)
    if (length(members) > 1) {
      d <- as.matrix(bc_dist)[members, members]
      total <- total + sum(d[upper.tri(d)]) / choose(length(members), 2)
    }
  }
  wss[i] <- total
}

# Plot elbow
elbow_df <- data.frame(k = k_range, within_cluster_dissimilarity = wss)

ggplot(elbow_df, aes(x = k, y = within_cluster_dissimilarity)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Elbow Method for Hierarchical Clustering",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Dissimilarity"
  )


set.seed(42)
nmds <- metaMDS(mmsi_matrix, distance = "bray", k = 2, trymax = 100)

# Plot
scores_df <- as.data.frame(scores(nmds))
scores_df$cluster <- factor(cutree(hc, k = 5))  # overlay clusters if you want

ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "NMDS of Fishing Effort Composition")
