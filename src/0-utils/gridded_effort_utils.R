library(sf)
library(dplyr)


get_gridded_effort <- function(
    shape,               # shapefile of the EEZ (defines the limits of the new grid)
    fishing_effort,      # GFW apparent fishing effort that we want to re-sample
    cell_size = 0.05     # New grid cell size in degree
    ) {
  
  # Create a regular rectangular grid containing the whole eez
  bbox <- st_bbox(shape)
  
  grid <- st_make_grid(
    shape,
    cellsize = 0.05,
    what = "polygons"
  ) |> 
    st_as_sf() |> 
    rename(cell = x) |> 
    mutate(
      center = st_centroid(cell),
      center_lon = st_coordinates(center)[, 1],
      center_lat = st_coordinates(center)[, 2],
      cell_id = paste0(round(-center_lon, 2), "°W ", round(center_lat, 2), "°N")
    ) |> 
    dplyr::select(cell_id, cell)
  
  
  # Re-sample the fishing effort in the new grid
  fishing_sf <- st_as_sf(
    fishing_effort,
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  grid_effort <- grid |> 
    st_join(fishing_sf) |> 
    summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE), .by = c(mmsi, fishing_category, fishing_category_target, cell, cell_id))
  
  # Complete the grid so all ships have a complete set of cells
  mmsi_unique <- distinct(fishing_sf, mmsi)
  grid_unique <- distinct(grid, cell_id, cell)
  
  full_grid <- expand_grid(mmsi_unique, grid_unique)
  
  grid_effort_complete <- grid_effort |> 
    right_join(full_grid, by = c("mmsi", "cell_id", "cell")) |> 
    mutate(apparent_fishing_hours = replace_na(apparent_fishing_hours, 0))
  
  return(grid_effort_complete)
}