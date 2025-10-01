bbox <- st_bbox(fao34_sf)

data <- effort |> 
  mutate(
    distant_type = case_when(
      origin_type %in% c("FAO 34 distant (Other)", "Non-FAO 34 distant") ~ "Other distant",
      TRUE ~ origin_type
    )
  )

# Prepare RGB scaled data (log-transformed)
rgb_data <- data |>
  group_by(lat, lon, distant_type) |>
  summarise(fh = sum(apparent_fishing_hours), .groups = "drop") |>
  pivot_wider(names_from = distant_type, values_from = fh, values_fill = 0) |>
  mutate(
    fao = log1p(`FAO 34 distant (African)`),
    other = log1p(`Other distant`),
    dom = log1p(Domestic)
  )

# Normalize to max value across all channels to keep brightness proportional
max_val <- max(rgb_data$fao, rgb_data$other, rgb_data$dom, na.rm = TRUE)

rgb_scaled <- rgb_data |>
  mutate(
    R = dom / max_val,
    G = fao / max_val, 
    B = other / max_val,
    fill = rgb(R, G, B)
  )

# Plot
p <- ggplot(rgb_scaled) +
  geom_sf(data = fao34_sf, fill = "black", color = NA) +
  geom_raster(aes(x = lon, y = lat, fill = fill)) +
  scale_fill_identity() +  # Use actual RGB colors
  geom_sf(data = eez_overlap_sf, color = "white", fill = NA, size = 0.2) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = NA, color = NA),  # transparent
    panel.grid.major = element_line(color = "white", size = 0.3),
    panel.ontop = TRUE
  )

output_file <- "report/figures/map_effort_by_origin_type_rgb.png"
if (!file.exists(output_file)) {
  ggsave(output_file, plot = p, width = 12, height = 8, dpi = 300)
}