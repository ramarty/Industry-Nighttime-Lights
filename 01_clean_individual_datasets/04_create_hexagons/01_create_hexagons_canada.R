# Create Hexagons

# Create hexagons around all areas with lights or firms. Creates hexagons of 
# different sizes. 

# Coordinates with Firms / NTL -------------------------------------------------
#### Firms
firms <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

#### DMSP-OLS
dmspols_df <- map_df(FIRM_YEARS_CANADA, function(year){
  print(year)
  
  dmspols_yr <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", 
                                 paste0("canada_dmspols_",year,".tif")))
  
  dmspols_yr_df <- dmspols_yr %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() 
  
  names(dmspols_yr_df) <- c("value", "lon", "lat")
  
  dmspols_yr_df %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(lon, lat)
  
}) %>%
  distinct(lon, lat)

# Grab hexagon ids from NTL ----------------------------------------------------
coords_sf = st_as_sf(dmspols_df, coords = c("lon", "lat"), crs = 4326)

for(res_i in 1:8){
  print(res_i)
  coords_sf[[paste0("hexgrid", res_i)]] <- point_to_h3(coords_sf, res = res_i) 
}

# Make and export hexagons -----------------------------------------------------
make_hex_polygons <- function(res_i,
                              coords_sf,
                              firms){
  
  hex_ids <- c(coords_sf[[paste0("hexgrid", res_i)]], firms[[paste0("hexgrid", res_i)]]) %>%
    unique()
  
  hex_sf <- hex_ids %>%
    unique() %>%
    h3_to_polygon() %>%
    st_as_sf() %>%
    dplyr::rename(geometry = x)
  
  hex_sf$hexid <- hex_ids
  
  return(hex_sf)
}

for(i in 1:8){
  print(i)
  hex_sf <- make_hex_polygons(i, coords_sf, firms)
  
  hex_sf <- hex_sf %>%
    dplyr::rename(id = hexid)
  
  saveRDS(hex_sf, file.path(data_file_path, "Grid", "FinalData", "canada", "grids_blank",
                            paste0("hexgrid_", i, ".Rds")))
}






