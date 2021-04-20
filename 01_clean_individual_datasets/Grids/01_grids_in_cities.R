# Create Grids within Cities

# Load Cities ------------------------------------------------------------------
can_cities <- readRDS(file.path(data_file_path, "Cities in Canada", "FinalData", "can_cities.Rds"))
mex_cities <- readRDS(file.path(data_file_path, "Cities in Mexico", "FinalData", "mex_cities.Rds"))

# Grid For Cities --------------------------------------------------------------
# Extract NTL grid that intersects with a city location. Use both DMSP-OLS and 
# VIIRS grid. To extract grid:
# -- (1) Crop [not strickly needed, but helps speeds up masking by reducing size of data]
# -- (2) Mask to cities [if grid doesn't intersect with city, becomes NA]
# -- (3) rasterToPolygons [turns non-NA cells into polygon]
# -- (4) Indicate which city intersects with

#### Define Function
city_to_grid_i <- function(i, city_sp, r){
  # For the ith city in city_sp, grab the pixels in r as a polygon and add the
  # city name and id
  # ARGS
  # --i: ith city (row in city_sp)
  # --city_sp: city polygon, with uid and city_name variables
  # --r: raster
  print(paste(i, "/", nrow(city_sp)))
  
  city_sp_i <- city_sp[i,]
  
  r_in_city <- r %>% 
    raster::crop(city_sp_i) %>% 
    raster::mask(city_sp_i) %>%
    rasterToPolygons()
  
  r_in_city$city_uid  <- city_sp_i$city_uid
  r_in_city$city_name <- city_sp_i$city_name
  
  return(r_in_city)
}

#### Load Satellite Data
can_viirs <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", "can_viirs_median_2019.tif"))
mex_viirs <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", "mex_viirs_median_2019.tif"))

can_dmsp <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED", "Harmonized_DN_NTL_2018_simVIIRS_CAN.tif"))
mex_dmsp <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED", "Harmonized_DN_NTL_2018_simVIIRS_MEX.tif"))

#### Make Grids
can_viirs_sp <- lapply(1:nrow(can_cities), city_to_grid_i, can_cities, can_viirs) %>% do.call(what = "rbind")
mex_viirs_sp <- lapply(1:nrow(mex_cities), city_to_grid_i, mex_cities, mex_viirs) %>% do.call(what = "rbind")

can_dmsp_sp <- lapply(1:nrow(can_cities), city_to_grid_i, can_cities, can_dmsp) %>% do.call(what = "rbind")
mex_dmsp_sp <- lapply(1:nrow(mex_cities), city_to_grid_i, mex_cities, mex_dmsp) %>% do.call(what = "rbind")

#### Assign ID
can_viirs_sp$id <- 1:nrow(can_viirs_sp)
mex_viirs_sp$id <- 1:nrow(mex_viirs_sp)
can_dmsp_sp$id <- 1:nrow(can_dmsp_sp)
mex_dmsp_sp$id <- 1:nrow(mex_dmsp_sp)

# Export -----------------------------------------------------------------------
saveRDS(can_viirs_sp,    file.path(data_file_path, "Grid", "RawData","can_citygridviirs.Rds"))
saveRDS(mex_viirs_sp,    file.path(data_file_path, "Grid", "RawData","mex_citygridviirs.Rds"))

saveRDS(can_dmsp_sp,    file.path(data_file_path, "Grid", "RawData","can_citygriddmsp.Rds"))
saveRDS(mex_dmsp_sp,    file.path(data_file_path, "Grid", "RawData","mex_citygriddmsp.Rds"))


