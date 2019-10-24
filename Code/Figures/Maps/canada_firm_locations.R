# Download GADM

# Load/Prep Data ---------------------------------------------------------------
coords_df <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "coordinates_appended.Rds"))
can_adm0 <- readRDS(file.path(raw_data_file_path, "GADM", "gadm36_CAN_0_sp.rds"))

can_adm0 <- gSimplify(can_adm0, tol=.05)

# Map --------------------------------------------------------------------------
map <- ggplot() +
  geom_polygon(data=can_adm0, aes(x=long, y=lat, group=group), fill="gray60") +
  geom_point(data=coords_df, aes(x=lon, y=lat), color="red", size=.1) +
  theme_void() +
  coord_map(projection = "gilbert")
ggsave(map, filename = file.path(figures_file_path, "canada_firm_locations.png"), height=8, width=12)
