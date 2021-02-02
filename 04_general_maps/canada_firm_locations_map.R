# Plot firm locations

# To make the map look similar to the nighttime lights map, use dmspols as the
# basemap (instead of an administrative map)

# Load/Prep Data ---------------------------------------------------------------
coords_df <- readRDS(file.path(project_file_path, "Canada Industry Data", "FinalData", "firms_clean.Rds"))
gadm <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_CAN_1_sp.rds")) # %>% gSimplify(tol=.1)

gadm <- crop(gadm, extent(-141.003, -52.65917, 41.67693, 70))

coords_df <- as.data.frame(coords_df)

# Map --------------------------------------------------------------------------
map <- ggplot() +
  geom_polygon(data=gadm, aes(x=long, y=lat, group=group), fill="gray60") +
  geom_point(data=coords_df, aes(x=lon, y=lat), color="red", size=.1) +
  theme_void() +
  coord_quickmap()
ggsave(map, filename = file.path(figures_file_path, "canada_firm_locations.png"), height=8, width=12)
