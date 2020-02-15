# Create hexagons with data

# Create hexagons around all areas with light or a firm

# Load and Prep Data -----------------------------------------------------------
#### Firm Data
firmdata_df <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "points_data", "firms_points_data.Rds"))
firmdata_df <- firmdata_df %>%
  distinct(lon, lat)
firmdata_df$id <- 1:nrow(firmdata_df)

#### DMSP-OLS
dmspols <- raster(file.path(raw_data_file_path, "Nighttime Lights", "DMSPOLS", "canada_dmspols_2013.tif"))
dmspols.df <- as(dmspols, "SpatialPixelsDataFrame")
dmspols.df <- as.data.frame(dmspols.df)
colnames(dmspols.df) <- c("value", "x", "y") 
dmspols.df <- dmspols.df[dmspols.df$value > 0,]
dmspols.df <- dmspols.df %>%
  dplyr::select(x, y) %>%
  dplyr::rename(lon = x,
                lat = y) %>%
  mutate(id = 1:n())

#### Coordinates
coords_df <- bind_rows(firmdata_df, dmspols.df)

#### Round Coordinates
# Round coordinates to the first decimal place. Maximum displacement (if floor)
# is: 1.19 --> 1.1, changed by 0.09, rounds to .1, where .1*111.12 = 11.12. So,
# to be extra safe, buffer by 15. 
coords_rounds_df <- coords_df
coords_rounds_df$lat <- coords_rounds_df$lat %>% round(1)
coords_rounds_df$lon <- coords_rounds_df$lon %>% round(1)

coords_rounds_df <- coords_rounds_df %>%
  distinct(lat, lon) %>%
  mutate(id = 1:n())

#### Buffer
coordinates(coords_rounds_df) <- ~lon+lat
crs(coords_rounds_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
coords_buff <- gBuffer_chunks(coords_rounds_df, 20/111.12, 1000)
coords_buff$id <- 1
coords_buff <- raster::aggregate(coords_buff, by="id")

# Create Hexagons --------------------------------------------------------------
hexagons <- spsample(coords_buff, type="hexagonal", cellsize=10/111.12)
HexPols <- HexPoints2SpatialPolygons(hexagons)
HexPols$id <- 1:length(HexPols)

# Remove Hexagons that dont intersect with lights or firms ---------------------
coordinates(coords_df) <- ~lon+lat
crs(coords_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

HexPols_OVER_coords_df <- over_chunks(HexPols, coords_df, "sum", 500)

HexPols <- HexPols[!is.na(HexPols_OVER_coords_df$id),]

# Export -----------------------------------------------------------------------
saveRDS(HexPols, file.path(final_data_file_path, "Canada Industry Data", "hexagon_data", "hexagons.Rds"))
