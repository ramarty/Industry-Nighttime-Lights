# Plot firm locations

# To make the map look similar to the nighttime lights map, use dmspols as the
# basemap (instead of an administrative map)

# Load/Prep Data ---------------------------------------------------------------
coords_df <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "coordinates_appended.Rds"))
canada_gadm <- readRDS(file.path(raw_data_file_path, "GADM", "gadm36_CAN_0_sp.rds")) %>% gSimplify(tol=.1)

canada_gadm <- crop(canada_gadm, extent(-141.003, -52.65917, 41.67693, 70))

# Map --------------------------------------------------------------------------
map <- ggplot() +
  geom_polygon(data=canada_gadm, aes(x=long, y=lat, group=group), fill="gray60") +
  geom_point(data=coords_df, aes(x=lon, y=lat), color="red", size=.1) +
  theme_void() +
  coord_quickmap()
ggsave(map, filename = file.path(figures_file_path, "canada_firm_locations.png"), height=8, width=12)
