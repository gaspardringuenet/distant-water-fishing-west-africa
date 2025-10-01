library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

library(vegan)
library(betapart)

library(cluster)
library(scales)

library(ClustGeo)
library(geosphere)

library(progress)


# Function we want to have:
# - compute_dist : compute the distance matrix for clustering (if spatial constraint is desired computes two matrices)
# - silhouette_grid_search : compute the mean silhouette width for a grid of k (number of cluster) and alpha (spatial constraint)
# - assign_clusters : assign clusters using a 

# compute_dist ------------------------------------------------------------

compute_dist <- function(effort,
                         as_species = c("mmsi", "flag", "flotilla"),
                         as_abundance = c("h", "kWh"),
                         bin_to_0.5 = FALSE, 
                         spatial_constraint = FALSE,
                         diss = c("hellinger", "bray")) {
  
  # Match arguments
  as_species <- match.arg(as_species)
  as_abundance <- match.arg(as_abundance)
  diss <- match.arg(diss)
  
  species_column <- switch(as_species,
                           "mmsi" = effort$mmsi,
                           "flag" = effort$flag,
                           "flotilla" = paste(effort$flag, effort$gear_type, sep = "_"))
  
  abundance_column <- switch(as_abundance,
                             "h" = effort$apparent_fishing_hours,
                             "kWh" = effort$kWh)
  
  # Bin coordinates if required
  if (bin_to_0.5) {
    effort <- effort |> 
      mutate(
        lat = round(lat * 2) / 2,
        lon = round(lon * 2) / 2,
      )
  }
  
  # Create a data.frame with one row per cell, containing the "abundance" of each "species"
  site_species_df <- effort |>
    mutate(
      species = species_column,
      abundance = abundance_column
    ) |> 
    summarize(
      abundance = sum(abundance, na.rm = TRUE), 
      .by = c(lat, lon, species)
    ) |> 
    mutate(cell_id = paste(lat, lon, sep = "_"))
  
  species_matrix <- site_species_df |> 
    pivot_wider(names_from = species, values_from = abundance, values_fill = 0) |> 
    column_to_rownames("cell_id")
  
  cell_coords <- species_matrix |>  # Keep the coordinates of the cells for later
    dplyr::select(lat, lon)
  
  species_matrix <- species_matrix |>
    dplyr::select(-c(lat, lon))
  
  # Compute the community based dissimilarity matrix
  
  if (diss == "bray") {
    D0 <- vegan::vegdist(as.matrix(species_matrix), method = "bray")
  } else if (diss == "hellinger") {
    hellinger_mat <- decostand(as.matrix(species_matrix), method = "hellinger")
    D0 <- dist(hellinger_mat, method = "euclidean")
  }
  
  # Compute the spatial distance matrix (if spatial constraint is desired)
  D1 <- NULL
  if (spatial_constraint) {
    coords_mat <- as.matrix(cell_coords[, c("lon", "lat")])
    D1 <- distm(coords_mat) / 1000  # meters to km
    D1 <- as.dist(D1)               # Convert to dist object
  }
  
  return(list(
    species_matrix = species_matrix,
    cell_coords = cell_coords,
    D0 = D0,
    D1 = D1
  ))
  
}


# silhouette_grid_search --------------------------------------------------

silhouette_k_search <- function(D0, 
                                k_min = 2,
                                k_max = 30) {
  
  results <- data.frame(k = k_min:k_max)
  results$sil_width <- NA_real_
  
  total <- nrow(results)
  pb <- progress::progress_bar$new(
    total = total,
    format = "  [:bar] :current/:total eta: :eta",
    clear = FALSE,
    width = 60
  )
  
  for (i in seq_len(total)) {
    k_i <- results$k[i]
    
    # Build tree
    tree_i <- hclust(D0, method = "ward.D2")
    
    # Cut tree and compute silhouette on D0
    clusters_i <- cutree(tree_i, k = k_i)
    sil_i <- silhouette(clusters_i, D0)
    
    # Store average silhouette width
    results$sil_width[i] <- mean(sil_i[, 3])
    
    pb$tick()
  }
  
  best <- results[which.max(results$sil_width), ]
  
  plot <- ggplot(results, aes(x = k, y = sil_width)) +
    geom_line() + 
    geom_point() +
    labs(x = "Number of clusters (k)",
         y = "Average silhouette width") +
    theme_minimal()
  
  return(list(
    results = results,
    best = best,
    plot = plot
  ))
}


silhouette_grid_search_geo <- function(D0, 
                                       D1, 
                                       alpha_seq = seq(0, 1, by = 0.1), 
                                       k_min = 2,
                                       k_max = 30) {
  
  results <- expand.grid(alpha = alpha_seq, k = k_min:k_max)
  results$sil_width <- NA_real_
  
  total <- nrow(results)
  pb <- progress::progress_bar$new(
    total = total,
    format = "  [:bar] :current/:total eta: :eta",
    clear = FALSE,
    width = 60
  )
  
  for (i in seq_len(total)) {
    alpha_i <- results$alpha[i]
    k_i <- results$k[i]
    
    # Build tree with spatial constraint alpha_i
    tree_i <- hclustgeo(D0, D1, alpha = alpha_i)
    
    # Cut tree and compute silhouette on D0
    clusters_i <- cutree(tree_i, k = k_i)
    sil_i <- silhouette(clusters_i, D0)
    
    # Store average silhouette width
    results$sil_width[i] <- mean(sil_i[, 3])
    
    pb$tick()
  }
  
  best <- results[which.max(results$sil_width), ]

  plot <- ggplot(results, aes(x = k, y = sil_width, color = factor(alpha))) +
    geom_line() + 
    geom_point() +
    labs(x = "Number of clusters (k)",
         y = "Average silhouette width",
         color = "Alpha") +
    theme_minimal()
  
  return(list(
    results = results,
    best = best,
    plot = plot
  ))
}


# assign_clusters ---------------------------------------------------------

assign_clusters <- function(D0,
                            D1 = NULL,
                            cell_coords,
                            alpha = NULL,
                            k) {
  
  if (!inherits(D0, "dist")) stop("D0 must be a 'dist' object")
  if (!is.null(D1) && !inherits(D1, "dist")) stop("D1 must be a 'dist' object")
  if (!is.data.frame(cell_coords)) stop("cell_coords must be a data frame")
  if (nrow(cell_coords) != attr(D0, "Size")) {
    stop("Number of rows in 'cell_coords' must match number of observations in D0")
  }
  
  # Create hierarchical tree
  if (!is.null(D1) && !is.null(alpha)) {tree <- hclustgeo(D0, D1, alpha)}
  else {
    print("D1 and/or alpha absent - proceeding to clustering without spatial constraint...")
    tree <- hclust(D0, method = "ward.D2")
  }
  
  # Cut tree
  cluster_groups <- cutree(tree, k)
  
  # Add cluster IDs to coordinates
  clusters <- cbind(cell_coords, cluster = cluster_groups)
  
  plot <- ggplot(clusters, aes(x = lon, y = lat, fill = as.factor(cluster))) +
    geom_tile() +
    coord_fixed() +
    scale_fill_brewer(palette = "Set3", name = "Cluster") +
    theme_minimal()
  
  return(list(
    clusters = clusters,
    plot = plot
  ))
}
  

# summarize_clusters ------------------------------------------------------

summarize_clusters <- function(effort,
                               clusters, 
                               as_species = c("mmsi", "flag", "flotilla"),
                               as_abundance = c("h", "kWh"),
                               spat_res = c(0.1, 0.5)) {
  
  # Match arguments
  as_species <- match.arg(as_species)
  as_abundance <- match.arg(as_abundance)
  spat_res <- match.arg(as.character(spat_res), choices = c("0.1", "0.5"))
  spat_res <- as.numeric(spat_res)
  
  # Prepare species column
  effort <- effort |> 
    mutate(
      species = case_when(
        as_species == "mmsi" ~ as.character(mmsi),
        as_species == "flag" ~ flag,
        as_species == "flotilla" ~ paste(flag, gear_type, sep = "_")
      )
    )
  
  # Round lat/lon if 0.5° binning
  if (spat_res == 0.5) {
    effort <- effort |> 
      mutate(
        lat_bin = round(lat * 2) / 2,
        lon_bin = round(lon * 2) / 2
      )
    clusters <- clusters |> 
      rename(
        lat_bin = lat,
        lon_bin = lon
      )
    join_by_cols <- c("lat_bin", "lon_bin")
  } else {
    join_by_cols <- c("lat", "lon")
  }
  
  # Join effort with clusters
  effort_clust <- effort |> 
    inner_join(clusters, by = join_by_cols) |>
    filter(!is.na(species)) # Avoid NA species
  
  if (nrow(effort_clust) == 0) {
    stop("No overlapping cells between 'effort' and 'clusters'. Check coordinates and 'spat_res'.")
  }
  
  # Abundance column
  abundance <- sym(ifelse(as_abundance == "h", "apparent_fishing_hours", "kWh"))
  
  # Total effort per cluster
  cluster_totals <- effort_clust |> 
    summarize(
      total_effort = sum(!!abundance, na.rm = TRUE),
      n_cells = n(),
      area_m2 = n_cells * ((1852 * spat_res) ^ 2),
      effort_per_m2 = total_effort / area_m2,
      .by = cluster
    )
  
  # Diversity metrics
  species_matrix <- effort_clust |> 
    summarize(abundance = sum(!!abundance, na.rm = TRUE), .by = c(cluster, species)) |> 
    pivot_wider(names_from = species, values_from = abundance, values_fill = 0) |> 
    column_to_rownames("cluster")
  
  richness <- rowSums(species_matrix > 0)
  shannon <- vegan::diversity(species_matrix, index = "shannon")
  pielou <- shannon / log(richness)
  pielou[is.nan(pielou)] <- NA
  
  diversity_df <- tibble(
    cluster = as.numeric(rownames(species_matrix)),
    richness = richness,
    shannon = shannon,
    pielou = pielou
  )
  
  # Top-5 species per cluster
  top5_species <- effort_clust |> 
    summarize(effort = sum(!!abundance, na.rm = TRUE), .by = c(cluster, species)) |> 
    group_by(cluster) |> 
    arrange(cluster, dplyr::desc(effort)) |> 
    slice_head(n = 5) |> 
    mutate(rank = dplyr::row_number()) |> 
    ungroup()
  
  top5_species <- top5_species |> 
    left_join(cluster_totals |> dplyr::select(cluster, total_effort), by = "cluster") |> 
    mutate(pct_effort = 100 * effort / total_effort) |> 
    arrange(cluster, rank)
  
  top5_summary <- top5_species |> 
    mutate(
      species_effort_pct = paste0(species,
                                  " (",
                                  #round(effort, 1), 
                                  #", ", 
                                  round(pct_effort, 1), 
                                  "%)")
    ) |> 
    group_by(cluster) |> 
    summarise(top5 = paste(species_effort_pct, collapse = "; "))
  
  # Final summary
  summary_table <- cluster_totals |> 
    left_join(diversity_df, by = "cluster") |> 
    left_join(top5_summary, by = "cluster") |> 
    rename(id = cluster) |> 
    dplyr::select(
      id,
      total_effort,
      effort_per_m2,
      richness,
      shannon,
      pielou,
      top5
    )
  
  # Rename with units
  effort_unit <- ifelse(as_abundance == "h", "h", "kW.h")
  colnames(summary_table)[colnames(summary_table) == "effort_per_m2"] <- paste0(effort_unit, ".m-2.an-1")
  effort_unit <- paste0(effort_unit, ".an-1")
  colnames(summary_table)[colnames(summary_table) == "total_effort"] <- effort_unit
  
  return(summary_table)
}


#  Test -------------------------------------------------------------------

# source("src/0-utils/load_all_datasets.R")
# source("src/0-utils/clean_effort.R")
# 
# # Clean the apparent fishing effort dataset
# min_dist <- 5
# min_hours <- 2000
# 
# l <- clean_effort(effort, min_dist, min_hours)
# 
# total_initial_effort <- l[[1]]
# discarded_effort <- l[[2]]
# effort <- l[[3]]
# 
# 
# eez_areas <- eez_sf |>
#   filter(POL_TYPE != "Joint regime", !is.na(ISO_TER1)) |> 
#   mutate(area_m2 = drop_units(st_area(geometry))) |> 
#   dplyr::select(ISO_TER1, area_m2) |>
#   st_drop_geometry()
# 
# 
# x <- compute_dist(effort |> filter(distance_to_shore_km <= 200 * 1.852, gear_type_fr == "Chaluts"),
#                   as_species = "flag",
#                   as_abundance = "kWh",
#                   bin_to_0.5 = FALSE,
#                   spatial_constraint = TRUE,
#                   diss = "hellinger")
# 
# x <- compute_dist(effort,
#                   as_species = "flotilla",
#                   as_abundance = "kWh",
#                   bin_to_0.5 = TRUE,
#                   spatial_constraint = FALSE,
#                   diss = "hellinger")
# 
# y <- silhouette_grid_search_geo(x$D0, x$D1, k_max = 30)
# z <- silhouette_k_search(x$D0, k_max = 30)
# 
# y$plot
# z$plot
# 
# results <- assign_clusters(D0 = x$D0, 
#                            #D1 = x$D1,
#                            cell_coords = x$cell_coords, 
#                            k = 11,
#                            #alpha = 0.6
#                            )
# results$plot
# 
# 
# summary <- summarize_clusters(effort,
#                               clusters = results$clusters, 
#                               as_species = "flotilla",
#                               as_abundance = "kWh",
#                               spat_res = 0.5)
