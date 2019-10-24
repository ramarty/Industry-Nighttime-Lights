# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

# Load Data --------------------------------------------------------------------
dmspols <- raster(file.path(raw_data_file_path, "Nighttime Lights", "DMSPOLS", "canada_dmspols_2000.tif"))

# VIIRS Plot -----------------------------------------------------------------
dmspols.df <- as(dmspols, "SpatialPixelsDataFrame")
dmspols.df <- as.data.frame(dmspols.df)
colnames(dmspols.df) <- c("value", "x", "y") 

dmspols.df$value_adj <- (dmspols.df$value) %>% sqrt %>% sqrt
p <- ggplot() +
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
ggsave(p, filename = file.path(figures_file_path, "canada_dmspols_2000.png"), height=5, width=7)


