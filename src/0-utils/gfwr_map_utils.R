library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(glue)
library(ggplot2)

# Map theme with dark background
gfw_map_theme <- ggplot2::theme_minimal() + 
  ggplot2::theme(
    panel.border = element_blank(), 
    legend.position = "bottom", legend.box = "vertical", 
    legend.key.height = unit(3, "mm"), 
    legend.key.width = unit(20, "mm"),
    legend.text = element_text(color = "#848b9b", size = 8), 
    legend.title = element_text(face = "bold", color = "#363c4c", size = 8, hjust = 0.5), 
    plot.title = element_text(face = "bold", color = "#363c4c", size = 10), 
    plot.subtitle = element_text(color = "#363c4c", size = 10), 
    axis.title = element_blank(), 
    axis.text = element_text(color = "#848b9b", size = 6)
  )

# Palette for fishing activity
map_effort_light <- c("#ffffff", "#eeff00", "#3b9088","#0c276c")

gradient_gfw <- scale_fill_gradientn(
  trans = "log10",
  colors = map_effort_light,
  na.value = NA,
  labels = scales::comma
  )

gradient2 <- scale_fill_viridis_c(
  option = "inferno",
  trans = "log10",
  na.value = NA
  )

gradient3 <- scale_fill_viridis_c(
  option = "inferno",
  trans = "log10",
  na.value = "green"
)

# Plot function
plot_effort_map <- function(spatial_effort, eez_shp, start_date = "2022-01-01", end_date = "2024-12-31"){
  
  bbox <- st_bbox(eez_shp)
  
  spatial_effort |> 
    filter(apparent_fishing_hours >= 1) |> 
    ggplot() +
    geom_raster(aes(x = lon,
                    y = lat,
                    fill = apparent_fishing_hours)) +
    geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
    geom_sf(data = eez_shp, color = "black", fill = NA, size = 0.2) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    labs(subtitle = glue("{start_date} to {end_date}"),
         fill = "Fishing hours") +
    gradient2 +
    gfw_map_theme
}



