# Geographically Weighted Regression

set.seed(42)

# Load Data --------------------------------------------------------------------
grid <- readRDS(file.path(merged_data_grid, paste0("hex_25km_clean",".Rds")))
grid_sp <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0("hex_25km",".Rds")))

canada <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_CAN_0_sp.rds"))
canada <- gSimplify(canada, tol = 0.1)
canada$id <- 1
canada <- canada %>% spTransform(PROJ_canada)


grid <- grid[grid$year %in% 2001,]

coordinates(grid) <- ~lon+lat
crs(grid) <- CRS(PROJ_canada)

grid$dmspols_var <- grid$dmspol_mean_log
grid$firm_var    <- grid$employment_sum_all_log

grid <- grid[!is.na(grid$dmspols_var),]
#grid <- grid[!is.na(grid$firm_var),]
grid$firm_var[is.na(grid$firm_var)] <- 0

lm1 <- lm(firm_var ~ dmspols_var, data=grid)

GWRbandwidth <- gwr.sel(firm_var ~ dmspols_var, data=grid, adapt=T) 

gwr.model = gwr(firm_var ~ dmspols_var, data=grid, adapt=GWRbandwidth, hatmatrix=F, se.fit=F) 

## Results
gwr_results <- gwr.model$SDF %>% as.data.frame()
gwr_results$id <- grid$id
gwr_results$lat <- NULL
gwr_results$lon <- NULL

gwr_results_sp <- merge(grid_sp, gwr_results, by="id")

##
gwr_results_sp$id <- row.names(gwr_results_sp)
gwr_results_sp_tidy <- gwr_results_sp %>% tidy()
gwr_results_sp_tidy <- merge(gwr_results_sp_tidy, gwr_results_sp@data, by="id")

gwr_results_sp_tidy <- gwr_results_sp_tidy[!is.na(gwr_results_sp_tidy$dmspols_var),]
p <- ggplot() +
  geom_polygon(data = canada,
               aes(x=long, y=lat, group=group),
               fill = NA, color="black") + 
  geom_polygon(data=gwr_results_sp_tidy,
             aes(x=long, y=lat, group=group, 
                 color=dmspols_var,
                 fill=dmspols_var)) +
  theme_void() +
  scale_colour_viridis() +
  scale_fill_viridis() +
  labs(fill = "Coef on NTL",
       color = "Coef on NTL")

ggsave(p, filename = file.path(figures_file_path, paste0("gwr_level_2001.png")), 
       height=7, width=8) 


