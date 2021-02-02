# Plot firm locations

# To make the map look similar to the nighttime lights map, use dmspols as the
# basemap (instead of an administrative map)

set.seed(42)

# Load/Prep Data ---------------------------------------------------------------
coords_df <- readRDS(file.path(project_file_path, "Data", "Mexico Industry Data", "FinalData", "firms_clean.Rds"))
gadm <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_MEX_0_sp.rds")) # %>% gSimplify(tol=.1)

gadm <- gSimplify(gadm, tol = .01)

# Large dataset, so sample
coords_df_sub <- coords_df[sample(1:nrow(coords_df), size=100000),]
coords_df_sub <- as.data.frame(coords_df_sub)

# Map --------------------------------------------------------------------------
map <- ggplot() +
  geom_polygon(data=gadm, aes(x=long, y=lat, group=group), fill="gray60") +
  geom_point(data=coords_df_sub, aes(x=lon, y=lat), color="red", size=.1) +
  theme_void() +
  coord_quickmap()
ggsave(map, filename = file.path(figures_file_path, "mexico_firm_locations.png"), height=8, width=12)
