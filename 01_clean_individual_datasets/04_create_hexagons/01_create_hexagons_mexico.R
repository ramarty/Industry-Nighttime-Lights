# Create Hexagons

# Create hexagons around all areas with lights or firms. Creates hexagons of 
# different sizes. The main challenge the code deals with is making hexagons
# across a large extent of Canada. To addess, it

# (1) Creates simplified area of where firms/light are
#   (a) Grabs coordinates of all locations with light and firms
#   (b) Rounds coordinates to the nearest 1km
#   (c) Buffers coordinates by 5km
# (2) First creates hexagons with 1000km buffer for Canada
# (3) Subset hexagon to those that intersect with above layer (firm+light layer)
# (4) Creates hexagons with 500km buffer for above layer
# (5) Repeat -  this ensures we only create hexagons around areas where firms/light
#     are, and we use the larger hexagons to minimize initial number of hexagons
#     made originally. 

# Coordiantes with Firms / NTL -------------------------------------------------
#### Firms
firms_1 <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_04_14.Rds"))
firms_1 <- firms_1 %>%
  dplyr::select(contains("hexgrid"))

firms_2 <- readRDS(file.path(data_file_path, "Mexico Industry Data", "FinalData", "firms_17_20.Rds"))
firms_2 <- firms_2 %>%
  dplyr::select(contains("hexgrid"))

firms <- bind_rows(firms_1, firms_2)

#### DMSP-OLS
dmspols_df <- map_df(c(2004, 2009, 2013), function(year){
  print(year)
  
  dmspols_yr <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", 
                                 paste0("mexico_dmspols_",year,".tif")))
  
  dmspols_yr_df <- dmspols_yr %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() 
  
  names(dmspols_yr_df) <- c("value", "lon", "lat")
  
  dmspols_yr_df %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(lon, lat)
  
}) %>%
  distinct(lon, lat)

#### VIIRS
viirs_df <- map_df(c(2017, 2018, 2019, 2020), function(year){
  print(year)
  
  viirs_yr <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", 
                                 paste0("mex_viirs_corrected_median_",year,".tif")))
  
  viirs_yr_df <- viirs_yr %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() 
  
  names(viirs_yr_df) <- c("value", "lon", "lat")
  
  viirs_yr_df %>%
    dplyr::filter(value >= 0.5) %>%
    dplyr::select(lon, lat)
  
}) %>%
  distinct(lon, lat)

#### Coordinates
coords_df <- bind_rows(dmspols_df, 
                       viirs_df) %>%
  distinct(lon, lat) %>%
  mutate(id = 1:n())

# Grab hexagon ids from NTL ----------------------------------------------------
coords_sf = st_as_sf(coords_df, coords = c("lon", "lat"), crs = 4326)

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
  
  saveRDS(hex_sf, file.path(data_file_path, "Grid", "FinalData", "mexico", "grids_blank",
                            paste0("hexgrid_", i, ".Rds")))
}

