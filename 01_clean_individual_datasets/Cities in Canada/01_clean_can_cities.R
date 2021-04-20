# Clean Canada Cities

# Load Data --------------------------------------------------------------------
can_cities <- readOGR(file.path(data_file_path, "Cities in Canada", "RawData", "lpc_000b16a_e.shp"))
can_cities <- can_cities %>% spTransform(CRS("+init=epsg:4326"))

# Aggregate --------------------------------------------------------------------
can_cities$PCUID <- can_cities$PCUID %>% as.character() %>% as.numeric()

## Aggregate Polygon
can_cities_agg <- raster::aggregate(can_cities, by = "PCUID")

## Aggregate Data
can_cities_agg_df <- can_cities@data %>%
  dplyr::group_by(PCUID) %>%
  dplyr::summarise_at(vars(PRNAME, PCNAME), . %>% unique %>% paste(collapse = " // "))

## Merge Polygon/Data
can_cities_agg <- merge(can_cities_agg, 
                        can_cities_agg_df, 
                        by = "PCUID")

# Cleanup ----------------------------------------------------------------------

## Make unique city name
can_cities_agg$city_name <- paste0(can_cities_agg$PRNAME, " - ",
                                   can_cities_agg$PCNAME, " - ",
                                   can_cities_agg$PCUID)

## Only keep select variables
can_cities_agg@data <- can_cities_agg@data %>%
  dplyr::select(PCUID, city_name) %>%
  dplyr::rename(city_uid = PCUID)

# Export -----------------------------------------------------------------------
saveRDS(can_cities_agg, file.path(data_file_path, "Cities in Canada", "FinalData",
                                  "can_cities.Rds"))

## Checks
can_cities_agg$uid %>% table %>% table
can_cities_agg$city_name %>% table %>% table

can_cities_agg$uid %>% is.na %>% table()
can_cities_agg$city_name %>% is.na %>% table()

