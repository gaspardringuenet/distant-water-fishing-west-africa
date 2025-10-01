# Load packages
library(tidyverse)
library(ggplot2)
library(vegan)

# Helper functions
source("src/0-utils/fleet_ecoregionalization_utils.R")

# Load data
x1 <- readRDS(file = "output/data/dist_largescale.rds")
str(x1)

# Check how well the data clusters
hellinger_mat1 <- decostand(x1$species_matrix, method = "hellinger")

set.seed(123)
sample_rows <- sample(1:nrow(hellinger_mat1), 2000)  # 1000 is manageable
hellinger_sub <- hellinger_mat1[sample_rows, ]

#nmds1 <- metaMDS(hellinger_sub, distance = "euclidean", k = 2, trymax = 10)
#plot(nmds1, display = "sites")

#nmds1 <- metaMDS(hellinger_mat1, distance = "euclidean", k = 2)



# TRAWLERS ----------------------------------------------------------------

# Load data
x2 <- readRDS(file = "output/data/dist_trawlers.rds")

# Check how well the data clusters
hellinger_mat2 <- decostand(x2$species_matrix, method = "hellinger")

set.seed(123)
sample_rows <- sample(1:nrow(hellinger_mat2), 1000)  # 1000 is manageable
hellinger_sub <- hellinger_mat2[sample_rows, ]

nmds2 <- metaMDS(hellinger_sub, distance = "euclidean", k = 2, trymax = 10)

nmds_results <- scores(nmds2, display = "sites")
cell_id <- rownames(nmds_results)
nmds_results <- as_tibble(nmds_results)
nmds_results$cell_id <- cell_id

clustering_results <- assign_clusters(x2$D0, x2$D1, x2$cell_coords, alpha = 0.5, k = 9)

centers <- clustering_results$clusters |>    # Reorder clusters for consistency between runs and interpretability
  summarize(
    lat = mean(lat),
    .by = cluster
  ) |> 
  arrange(desc(lat)) |> 
  mutate(
    ordered_cluster = row_number()
  ) |> 
  dplyr::select(cluster, ordered_cluster)

clustering_results$clusters <- clustering_results$clusters |> 
  left_join(centers, 
            by = "cluster") |> 
  dplyr::select(-cluster) |> 
  rename(cluster = ordered_cluster) |> 
  relocate(cluster)

nmds_results <- nmds_results |> 
  separate(cell_id, into = c("lat", "lon"), sep = "_", convert = TRUE) |> 
  left_join(
    y2$clusters,
    by = c("lat", "lon")
  ) 


ggplot(nmds_results, aes(x = NMDS1, y = NMDS2, fill = as.factor(cluster))) +
  geom_point(shape = 21, size = 3) +
  scale_fill_brewer(palette = "Set3", name = "Cluster") +
  theme_minimal()





#nmds1 <- metaMDS(hellinger_mat1, distance = "euclidean", k = 2)









# source("src/0-utils/clean_effort.R")

# x2 <- readRDS(file = "output/data/dist_trawlers.rds")

# Load data
# source("src/0-utils/load_all_datasets.R")
# 
# min_dist <- 5
# min_hours <- 2000
# l <- clean_effort(effort, min_dist, min_hours)
# effort <- l[[3]]


# # ...
# scores_sites <- scores(nmds, display = "sites")
# scores_df <- as.data.frame(scores_sites) %>%
#   mutate(cell_id = rownames(fg_matrix)) %>%
#   separate(cell_id, into = c("lat", "lon"), sep = "_", convert = TRUE)
# 
# # Map NMDS1 axis
# scores_df$NMDS1_scaled <- rescale(scores_df$NMDS1)
# 
# ggplot() +
#   geom_tile(data = scores_df, aes(x = lon, y = lat, fill = NMDS1_scaled)) +
#   scale_fill_viridis_c(name = "Trawler Gradient") +
#   geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
#            ylim = c(bbox["ymin"], bbox["ymax"])) +
#   theme_minimal() +
#   labs(title = "Gradient of Trawler Community Composition (NMDS1)")