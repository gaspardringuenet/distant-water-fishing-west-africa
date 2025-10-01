library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(terra)
library(raster)
library(scales)
library(tidyterra)
library(ggnewscale)
library(janitor)
library(ggpattern)

# Helper function
source("src/0-utils/gfwr_map_utils.R")

# Load shapefiles
fao34_sf <- st_read("data/shapefiles/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp") |> filter(F_CODE == 34)
eez_sf <- st_read("data/shapefiles/FAO34_EEZ/eez_fao34.shp")
eez_overlap_sf <- eez_sf |> st_intersection(fao34_sf |> dplyr::select(geometry))
bathy <- rast("data/bathymetry.tif")
lme_sf <- st_read("data/shapefiles/lme-large-marine-ecosystems-main/polygons/shp/lme66.shp") |> clean_names() |>  filter(lme_number %in% c(27, 28))
  

cecaf_north <- c("MAR", "ESH", "MRT", "SEN", "GMB")
cecaf_south <- c("CPV", "GNB", "GIN", "SLE", "LBR", "CIV", "GHA", "TGO", "BEN", "NGA", "CMR", "GNQ", "STP", "GAB", "COG", "COD", "AGO")

eez_overlap_sf <- eez_overlap_sf |> 
  mutate(
    is_cecaf = case_when(
      ISO_TER1 %in% cecaf_north | TERRITORY1 == "Canary Islands" ~ "COPACE sous-groupe Nord",
      ISO_TER1 %in% cecaf_south ~ "COPACE sous-groupe Sud",
      TRUE ~ "Hors évaluations COPACE"
    )
  )


lme_sf <- lme_sf |> 
  mutate(
    lme_name_fr = case_when(
      lme_name == "Canary Current" ~ "Courant des Canaries",
      lme_name == "Guinea Current" ~ "Courant de Guinée"
    )
  )


crs(bathy) <- "EPSG:4326"  # safer with terra
bathy_cropped <- crop(bathy, ext(fao34_sf))
bathy_masked <- mask(bathy_cropped, fao34_sf)
bathy_masked[bathy_masked > 0] <- NA
bathy_df <- as.data.frame(bathy_masked, xy = TRUE, na.rm = TRUE)
colnames(bathy_df)[3] <- "bathymetry"

bathy_df$bathy_bin <- cut(
  bathy_df$bathymetry,
  breaks = c(-8000, -1000, -200, 0),
  labels = c("> 1000 m", "200–1000 m", "0–200 m"),
  include.lowest = TRUE,
  right = FALSE
)
bathy_df$bathy_bin <- factor(bathy_df$bathy_bin, levels = c( "0–200 m", "200–1000 m", "> 1000 m"))

# Hillshading
#slope <- terrain(bathy_masked, v = "slope", unit = "radians")
#aspect <- terrain(bathy_masked, v = "aspect", unit = "radians")
#hillshade <- shade(slope, aspect, angle = 45, direction = 270)
#hill_df <- as.data.frame(hillshade, xy = TRUE)
#names(hill_df) <- c("x", "y", "hillshade")
#hill_df$amplified <- hill_df$hillshade

min_d <- min(bathy_df$bathymetry)

bbox <- st_bbox(fao34_sf)

map_theme <- ggplot2::theme_minimal() + 
  ggplot2::theme(
    panel.border = element_blank(), 
    legend.text = element_text(size = 8), 
    legend.title = element_text(face = "bold", size = 8, hjust = 0.5), 
    plot.title = element_text(face = "bold", size = 10), 
    plot.subtitle = element_text(size = 10), 
    axis.text = element_text(size = 6)
  )

p <- ggplot() +
  #geom_raster(data = hill_df, aes(x = x, y = y, fill = amplified), alpha = 1) +
  #scale_fill_gradient(low = "black", high = "white", guide = "none") +
  
  new_scale_fill() +
  geom_sf(data = lme_sf, aes(fill = lme_name_fr), color = NA, alpha = 0.3) +
  scale_fill_manual(
    values = c("Courant des Canaries" = "yellow",
               "Courant de Guinée" = "green"),
    name = "Large Marine Ecosystems"
  ) +
  
  new_scale_fill() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = bathy_bin), alpha = 1) +
  scale_fill_manual(
    values = c(
      "0–200 m" = "beige",
      "200–1000 m" = "brown4",
      "> 1000 m" = NA
    ),
    name = "Bathymétrie (m)",
    na.value = NA
  ) +
  
  geom_sf_pattern(data = subset(eez_overlap_sf, GEONAME != "Joint regime area Senegal / Guinea Bissau"),
                  aes(color = is_cecaf, 
                      pattern_fill = is_cecaf),
                  pattern = "circle",
                  pattern_color = NA,
                  pattern_spacing = 0.01,
                  pattern_size = 0.1,
                  fill = NA,
                  alpha = 1) +
  scale_color_manual(
    values = c("COPACE sous-groupe Nord" = "orange",
               "COPACE sous-groupe Sud" = "darkgreen",
               "Hors évaluations COPACE" = "grey"),
    name = "ZEE d'évaluation des stocks côtiers"
  ) +
  scale_pattern_fill_manual(
    values = c("COPACE sous-groupe Nord" = "orange",
               "COPACE sous-groupe Sud" = "darkgreen",
               "Hors évaluations COPACE" = "grey"),
    name = "ZEE d'évaluation des stocks côtiers"
  ) +
  
  geom_sf(data = fao34_sf, color = "black", fill = NA) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium"), fill = "lightgrey") +
  
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(x = "Longitude", y = "Latitude") +
  map_theme
  

output_file <- "report/figures/lme_eez_map.png"
ggsave(output_file, plot = p, width = 12, height = 8, dpi = 300)

s#breaks = c(seq(-8000, -1000, 1000), 500, seq(-400, 0, 100))
# scale_fill_hypso_tint_c(
#   palette = "etopo1_bathy", 
#   name = "Bathymetry (m)",
#   limits = c(-8000, 0)
# ) +
  
# p <- ggplot() +
#   geom_raster(data = hill_df, aes(x = x, y = y, fill = hillshade), alpha = 0.7) +
#   scale_fill_gradient(low = "white", high = "black", guide = "none") +  # hillshade background
#   #geom_raster(data = bathy_df, aes(x = x, y = y, fill = bathymetry), alpha = 0.5) +
#   #scale_fill_continuous(type = "gradient", name = "Bathymetry (m)") +
#   geom_sf(data = eez_overlap_sf, color = "white", fill = NA, size = 0.2) +
#   geom_sf(data = fao34_sf, color = "black", fill = NA, size = 1) +
#   geom_sf(data = ne_countries(returnclass = "sf", scale = "medium"), fill = "lightgrey") +
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
#            ylim = c(bbox["ymin"], bbox["ymax"])) +
#   labs(x = "Longitude", y = "Latitude") +
#   map_theme