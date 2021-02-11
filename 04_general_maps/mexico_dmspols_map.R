# Map of NTL in Mexico

# Load Data --------------------------------------------------------------------
dmspols <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "DMSPOLS", "mexico_dmspols_2000.tif"))
gadm <- readRDS(file.path(project_file_path, "Data", "GADM", "RawData", "gadm36_MEX_1_sp.rds")) 

# VIIRS Plot -----------------------------------------------------------------
dmspols.df <- as(dmspols, "SpatialPixelsDataFrame")
dmspols.df <- as.data.frame(dmspols.df)
colnames(dmspols.df) <- c("value", "x", "y") 

dmspols.df$value_adj <- (dmspols.df$value) %>% sqrt %>% sqrt
dmspols.df <- dmspols.df[dmspols.df$value_adj >= 0.6,]

p <- ggplot() +
  geom_polygon(data=gadm, aes(x=long, y=lat, group=group), fill="black") +
  geom_tile(data=dmspols.df, aes(x=x,y=y,fill=value_adj)) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  labs(title="") +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "black", mid="orange", high = "white",midpoint=1.9,
                       limits = c(0.6258,3.2388),
                       na.value = 'black') +
  theme(legend.position = "none") +
  coord_quickmap()
ggsave(p, filename = file.path(figures_file_path, "mexico_dmspols_2000.png"), height=8, width=12)


