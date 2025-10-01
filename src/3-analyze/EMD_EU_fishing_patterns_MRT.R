library(cluster)
library(dplyr)
library(emdist)
library(ggdendro)
library(ggplot2)
library(infotheo)
library(MASS)
library(patchwork)
library(purrr)
library(readr)
library(reshape2)
library(Rtsne)
library(sf)
library(stringr)
library(tidyr)


fig_save_path <- "output/figures/effort_patterns_EU/clustering_MRT/"


# Functions ---------------------------------------------------------------

source("src/0-utils/gfwr_map_utils.R")
source("src/0-utils/gridded_effort_utils.R")

# Function to convert gridded data into a matrix
convert_to_matrix <- function(data) {
  data$centroid <- st_centroid(data$cell)
  
  coords <- st_coordinates(data$centroid)
  data$lon <- round(coords[, 1], 2)
  data$lat <- round(coords[, 2], 2)
  
  grid_matrix <- acast(data, lat ~ lon, value.var = "apparent_fishing_hours", fill = 0)
  
  # Normalize the matrix to sum to 1 for EMD
  grid_matrix <- grid_matrix / sum(grid_matrix)
  
  return(grid_matrix)
}

# Function to compute EMD between two matrices
compute_emd <- function(mat1, mat2) {
  # Convert matrices into "point-weight" format for transport
  n1 <- which(mat1 > 0, arr.ind = TRUE)
  n2 <- which(mat2 > 0, arr.ind = TRUE)
  
  w1 <- mat1[n1]
  w2 <- mat2[n2]
  
  # Create transport problem format
  p1 <- data.frame(x = n1[, 1], y = n1[, 2], mass = w1)
  p2 <- data.frame(x = n2[, 1], y = n2[, 2], mass = w2)
  
  # Compute EMD
  emd_val <- emd2d(p1, p2)
  return(emd_val)
}

# Function to visualize dendrograms
plot_dendro <- function(dendro_data, label_data,
                        clustering = "hc.ward.5.class",
                        reference = "fishing_category",
                        method = "ward.D2", k = 5, cut_height = 30){
  
  rect_data <- label_data |> 
    group_by(.data[[clustering]]) |> 
    summarize(xmin = min(x), xmax = max(x), .groups = "drop")
  
  ggplot() +
    geom_segment(
      data = dendro_data$segments, 
      aes(x = x, y = y, xend = xend, yend = yend)
    ) +
    geom_text(
      data = label_data,
      aes(x = x, y = y, label = vessel_name, color = .data[[reference]]), 
      hjust = 1, size = 3,
      show.legend = FALSE
    ) +
    geom_point(
      data = label_data,
      aes(x = x, y = y, color = .data[[reference]]),
      size = 0
    ) +
    geom_rect(
      data = rect_data, 
      aes(xmin = xmin - 0.5, xmax = xmax + 0.5, group = .data[[clustering]]),
      ymin = -23, ymax = cut_height,
      fill = NA,
      color = "red"
    ) +
    scale_y_continuous(limits = c(-20, 80)) +
    coord_flip() +
    theme_classic() +
    labs(
      x = "", y = "Height",
      title = paste0("Hierarchical clustering of the spatial effort distributions (method = ", method, "; k = ", k, ")"),
      subtitle = "EU fleet in Mauritania (2022-2024) - Earth mover's distance used on 0.05° gridded effort maps.",
      color = reference,
      fill = "Cluster"
    ) +
    theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 3, shape = 15)))
}

# Load data ---------------------------------------------------------------

eu_fishing_in_agreements <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_EU_vessels_within_FPA_authorisations_DAILY_2022-2024.csv") |>  filter(eez == "MRT")

eez_shp <- st_read("data/shapefiles/EEZ/eez_v11.shp") |> 
  filter(ISO_TER1 == "MRT") |> 
  st_transform(crs = st_crs(4326)) 

bbox <- st_bbox(eez_shp)


# Resample the effort in a regular grid over the MRT EEZ ------------------

gridded_effort <- get_gridded_effort(shape = eez_shp,
                                     fishing_effort = eu_fishing_in_agreements,
                                     cell_size = 0.05)


# Convert each vessel's effort into a matrix object -----------------------

vessels <- unique(gridded_effort$mmsi)

vessel_matrices <- lapply(vessels, function(v) {
  one_vessel_grid <- gridded_effort |> filter(mmsi == v)
  convert_to_matrix(one_vessel_grid)
})

names(vessel_matrices) <- vessels

# Compute EMD between matrices --------------------------------------------

# Compute the distance matrix
emd_matrix <- matrix(0, nrow = length(vessels), ncol = length(vessels), dimnames = list(vessels, vessels))

for (i in 1:length(vessels)) {
  for (j in i:length(vessels)) {
    emd_matrix[i, j] <- emd2d(vessel_matrices[[i]], vessel_matrices[[j]], max.iter = 1000)
    emd_matrix[j, i] <- emd_matrix[i, j]  # Symmetric matrix
  }
}

# Convert to a distance object (optional, for clustering or MDS)
emd_dist <- as.dist(emd_matrix)

# Visualize the distance matrix

emd_matrix_named <- emd_matrix
vessel_names <- data.frame(mmsi = vessels) |> 
  left_join(eu_fishing_in_agreements |> distinct(mmsi, vessel_name), by = "mmsi") |> 
  pull(vessel_name)

attr(emd_matrix_named, "dimnames") <- list(vessel_names, vessel_names)

# heatmap(emd_matrix_named,
#         symm = TRUE,
#         keep.dendro = FALSE)

emd_dist_named <- as.dist(emd_matrix_named)

# Visualize distances -----------------------------------------------------

info <- data.frame(mmsi = as.numeric(names(emd_dist))) |> 
  left_join(eu_fishing_in_agreements |> distinct(mmsi, vessel_name, fishing_category, fishing_category_target),
            by = "mmsi")

# Non-Metric MDS
nmds <- isoMDS(emd_dist_named, k = 2)

nmds_df <- data.frame(
  X = nmds$points[, 1],
  Y = nmds$points[, 2],
  mmsi = as.numeric(attr(emd_dist, "Labels"))
) |> 
  left_join(info, by = "mmsi")

nmds_df |> 
  ggplot(aes(x = X, y = Y, color = fishing_category)) +
  geom_point() +
  labs(x = "", y = "", 
       color = "Fishing category",
       title = paste0("n-MDS plot of the spatial effort distributions (stress = ",round(nmds$stress, 2), ")"),
       subtitle = "EU fleet in Mauritania (2022-2024) - Earth mover's distance used on 0.05° gridded effort maps.")
ggsave(paste0(fig_save_path, "nmds_plot_MRT.png"),
       height = 15,
       width = 25,
       units = "cm")

# Stress is very high, we should use another way : t-SNE
set.seed(42)
tsne <- Rtsne(as.matrix(emd_dist),
              is_distance = TRUE, 
              perplexity = 10,
              verbose = TRUE,
              max_iter = 1e4)

clustering_df <- data.frame(
  X = tsne$Y[, 1],
  Y = tsne$Y[, 2],
  mmsi = as.numeric(attr(emd_dist, "Labels"))
) |> 
  left_join(info, by = "mmsi")

clustering_df |> 
  ggplot(aes(x = X, y = Y, color = fishing_category)) +
  geom_point() +
  labs(x = "", y = "", 
       color = "Fishing category",
       title = paste0("t-SNE plot of the spatial effort distributions - perplexity: ", tsne$perplexity),
       subtitle = "EU fleet in Mauritania (2022-2024) - Earth mover's distance used on 0.05° gridded effort maps.")
ggsave(paste0(fig_save_path, "t-SNE_plot_MRT.png"),
       height = 15,
       width = 25,
       units = "cm")

# Hierarchical clustering -------------------------------------------------

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(emd_dist, method = x)$ac
}

map_dbl(m, ac)

hc <- hclust(emd_dist, method = "ward.D2")
clusters <- cutree(hc, k = 3)

clustering_df <- data.frame(
  mmsi = as.numeric(names(clusters)),
  hc.ward.3.class = as.factor(clusters),
  hc.ward.5.class = as.factor(cutree(hc, k = 5)),
  hc.ward.6.class = as.factor(cutree(hc, k = 6)),
  hc.ward.7.class = as.factor(cutree(hc, k = 7)),
  hc.ward.10.class = as.factor(cutree(hc, k = 10))
) |> 
  left_join(info, by = "mmsi")


# Visualize dendrogram ----------------------------------------------------

dendro_data <- ggdendro::dendro_data(hc, type = "rectangle")

label_data <- dendro_data$labels |> 
  mutate(label = as.numeric(label)) |> 
  left_join(clustering_df, by = join_by(label == mmsi))

plot_dendro(dendro_data, label_data, clustering = "hc.ward.5.class", k = 5, cut_height = 30) + labs(color = "Fishing Category")
ggsave(paste0(fig_save_path, "dendro_hc.ward.5.class_fishcat.png"),
       height = 20,
       width = 30,
       units = "cm")
plot_dendro(dendro_data, label_data, clustering = "hc.ward.6.class", k = 6, cut_height = 26) + labs(color = "Fishing Category")
ggsave(paste0(fig_save_path, "dendro_hc.ward.6.class_fishcat.png"),
       height = 20,
       width = 30,
       units = "cm")
plot_dendro(dendro_data, label_data, clustering = "hc.ward.7.class", k = 7, cut_height = 20) + labs(color = "Fishing Category")
ggsave(paste0(fig_save_path, "dendro_hc.ward.7.class_fishcat.png"),
       height = 20,
       width = 30,
       units = "cm")
plot_dendro(dendro_data, label_data, clustering = "hc.ward.10.class", k = 10, cut_height = 16.6) + labs(color = "Fishing Category")
ggsave(paste0(fig_save_path, "dendro_hc.ward.10.class_fishcat.png"),
       height = 20,
       width = 30,
       units = "cm")

# Select number of classes ------------------------------------------------

# Elbow method using wthin-cluster sum of distance

emd_matrix <- as.matrix(emd_dist)

k_range <- 2:15
wcsd_fun <- function(clusters) {
  sum(sapply(unique(clusters$class), function(i) {
    
    mmsi_list <- clusters |> filter(class == i) |> pull(mmsi)
    idx <- match(mmsi_list, attr(emd_dist, "Labels"))
    
    if (length(idx) < 2) return(0)
    
    pairs <- combn(idx, 2, simplify = FALSE)
    sum(sapply(pairs, function(p) emd_matrix[p[1], p[2]]))
    
  }))
}

metrics <- lapply(k_range, function(k) {
  # Clustering with k classes
  clusters <- cutree(hc, k)
  clusters <- data.frame(
    mmsi = as.numeric(names(clusters)),
    class = as.factor(clusters)
  ) |> 
    left_join(info, by = "mmsi")
  
  # Computing metrics
  data.frame(
    k = k,
    wcsd = wcsd_fun(clusters), # Sum of distances
    U_fishcat_given_class =  mutinformation(clusters$class, clusters$fishing_category) / entropy(clusters$fishing_category), # U(Fishing Category | Clusters)
    U_class_given_fishcat = mutinformation(clusters$class, clusters$fishing_category) / entropy(clusters$class), # U(Clusters | Fishing Category)
    U_target_given_class =  mutinformation(clusters$class, clusters$fishing_category_target) / entropy(clusters$fishing_category_target), # U(Fishing Category Target | Clusters)
    U_class_given_target = mutinformation(clusters$class, clusters$fishing_category_target) / entropy(clusters$class) # U(Clusters | Fishing Category Target)
  )
}) |> 
  bind_rows()

metrics <-metrics |>
  pivot_longer(cols = setdiff(colnames(metrics), "k")) |> 
  mutate(
    name = case_when(
      name == "wcsd" ~ "Within-cluster sum of distances",
      name == "U_fishcat_given_class" ~ "U(Fishing Category | Clusters)",
      name == "U_class_given_fishcat" ~ "U(Clusters | Fishing Category)",
      name == "U_target_given_class" ~ "U(Target | Clusters)",
      name == "U_class_given_target" ~ "U(Clusters | Target)"
    )
  )

metrics |> 
  filter(name == "Within-cluster sum of distances") |> 
  ggplot(aes(x = k, y = value)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Number of classes",
    y = "WCSD",
    title = "Within-cluster sum of distance"
  ) +
  scale_x_continuous(breaks = seq(1, 15, 1))
ggsave(paste0(fig_save_path, "elbow_plot_wcsd.png"),
       height = 15,
       width = 20,
       units = "cm")

metrics |> 
  filter(name != "Within-cluster sum of distances") |> 
  ggplot(aes(x = k, y = value)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Number of classes",
    y = "",
    title = "Uncertainty coefficient (Theil's U)"
  ) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(1, 15, 1)) +
  facet_wrap(~ name)
ggsave(paste0(fig_save_path, "elbow_plots_uncertainty_coefficients.png"),
       height = 17,
       width = 22,
       units = "cm")

# We chose 7 classes

final_clustering_df <- clustering_df |> 
  dplyr::select(-ends_with(".class")) |> 
  mutate(class = clustering_df |> dplyr::select(hc.ward.7.class) |>  pull())


# Visualize spatial patterns of classes -----------------------------------

gridded_effort_by_class <- gridded_effort |> 
  left_join(final_clustering_df |> dplyr::select(mmsi, class),
            by = "mmsi") |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE), .by = c(cell, mmsi, class)) |> 
  summarize(
    effort_mean = mean(apparent_fishing_hours, na.rm = TRUE),
    effort_sd = sd(apparent_fishing_hours, na.rm = TRUE),
    n = n(),
    .by = c(cell, class)
  ) |> 
  mutate(
    coeff_of_var = effort_sd / effort_mean
  )

ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  geom_sf(data = eez_shp, fill = "lightblue", alpha = 0.3, color = "black") +
  geom_sf(data = gridded_effort_by_class, aes(fill = effort_mean), color = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title = "Average individual vessel effort per class",
    #subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31\n0.05° x 0.05° resampling",
    fill = "Fishing hours"
  ) +
  gradient2 +
  map_theme + 
  facet_wrap(~ class)
ggsave(paste0(fig_save_path, "effort_MRT-2022-2024_inFPA_by_cluster_class.png"),
       height = 30,
       width = 30,
       units = "cm")
