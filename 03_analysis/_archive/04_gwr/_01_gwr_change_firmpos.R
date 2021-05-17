# Geographically Weighted Regression

# Long Difference

set.seed(42)

# Load / Prep Data -------------------------------------------------------------
#### Hexagon Data
grid <- readRDS(file.path(merged_data_grid, paste0("hex_25km_clean",".Rds")))
grid <- grid %>%
  group_by(id) %>%
  mutate(dmspol_mean_2001 = dmspol_mean[year == 2001],
         dmspol_mean_log_2001 = dmspol_mean_log[year == 2001],
         N_firms_sum_all_2001 = N_firms_sum_all[year == 2001],
         N_firms_sum_all_log_2001 = N_firms_sum_all_log[year == 2001],
         employment_sum_all_2001 = employment_sum_all[year == 2001],
         employment_sum_all_log_2001 = employment_sum_all_log[year == 2001],
         
         dmspol_mean_2013 = dmspol_mean[year == 2013],
         dmspol_mean_log_2013 = dmspol_mean_log[year == 2013],
         N_firms_sum_all_2013 = N_firms_sum_all[year == 2013],
         N_firms_sum_all_log_2013 = N_firms_sum_all_log[year == 2013],
         employment_sum_all_2013 = employment_sum_all[year == 2013],
         employment_sum_all_log_2013 = employment_sum_all_log[year == 2013])

coordinates(grid) <- ~lon+lat
crs(grid) <- CRS(PROJ_canada)

#### Hexagon Shapefile
grid_sp <- readRDS(file.path(data_file_path, "Grid", "RawData", paste0("hex_25km",".Rds")))

#### Canada Shapefile
canada <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_CAN_0_sp.rds"))
canada <- gSimplify(canada, tol = 0.1)
canada$id <- 1
canada <- canada %>% spTransform(PROJ_canada)

# Prep for GWR -----------------------------------------------------------------
grid <- grid[grid$year %in% 2013,]

grid$dmspols_var <- grid$dmspol_mean_log_diff6
grid$firm_var    <- grid$employment_sum_all_log_diff6

grid <- grid[!is.na(grid$dmspols_var),]
grid <- grid[!is.na(grid$firm_var),]

grid <- grid[grid$firm_var > 0,]

# GWR Model --------------------------------------------------------------------
lm1 <- lm(firm_var ~ dmspols_var, data=grid)

GWRbandwidth <- gwr.sel(firm_var ~ dmspols_var, data=grid, adapt=T) 
gwr.model = gwr(firm_var ~ dmspols_var, data=grid, adapt=GWRbandwidth, hatmatrix=F, se.fit=F) 

# Grab & Format GWR Results ----------------------------------------------------
#### Results dataframe
gwr_results <- gwr.model$SDF %>% as.data.frame()
gwr_results$id <- grid$id
gwr_results$lat <- NULL
gwr_results$lon <- NULL

#### Rename everything except id
names(gwr_results)[!(names(gwr_results) %in% "id")] <- 
  paste0(names(gwr_results)[!(names(gwr_results) %in% "id")], "_gwr")

#### Merge grid data with GWR data 
grid_df <- merge(grid@data, gwr_results, by="id")

# GWR Coef Histogram -----------------------------------------------------------
p <- ggplot(grid_df) +
  geom_histogram(aes(x=dmspols_var_gwr)) +
  labs(x = "Coefficient on DMSP-OLS Across Cells")

ggsave(p, filename = file.path(figures_file_path, paste0("gwr_diff6_coef_hist_pos.png")), 
       height=7, width=8) 


# Regressions ------------------------------------------------------------------
### ALL CELLS
lm1 <- lm(dmspols_var_gwr ~ dmspol_mean_log_2001, data = grid_df) 
lm2 <- lm(dmspols_var_gwr ~ dmspol_sum_log_diff6, data = grid_df) 
lm3 <- lm(dmspols_var_gwr ~ employment_sum_all_log_2001, data = grid_df) 
lm4 <- lm(dmspols_var_gwr ~ employment_sum_all_log_diff6, data = grid_df)  
lm5 <- lm(dmspols_var_gwr ~ dmspol_mean_log_2001 + dmspol_sum_log_diff6 + employment_sum_all_log_2001 + employment_sum_all_log_diff6, data = grid_df) 

stargazer(lm1,
          lm2,
          lm3,
          lm4,
          lm5,
          dep.var.labels = "GWR Coefficient on log(DMSP-OLS)",
          covariate.labels = c("Log(NTL 2001)",
                               "Log(NTL 2013) - Log(NTL 2001)",
                               "Log(Employment 2001)",
                               "Log(Employment 2013) - Log(Employment 2001)"),
          dep.var.caption = "",
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          #report="vcs*",
          digits = 2,
          out=file.path(tables_file_path, paste0("predict_gwr_coef_changes_pos.tex")))

# GWR Coef Map -----------------------------------------------------------------
## SP Dataframe for map
grid_mapvars <- grid_df %>%
  dplyr::select(id, dmspols_var_gwr)

grid_sp_mapvars <- merge(grid_sp, grid_mapvars, by="id")
grid_sp_mapvars <- grid_sp_mapvars[!is.na(grid_sp_mapvars$dmspols_var_gwr),]

## Tidy
grid_sp_mapvars$id <- row.names(grid_sp_mapvars)
grid_sp_mapvars_tidy <- grid_sp_mapvars %>% tidy()
grid_sp_mapvars_tidy <- merge(grid_sp_mapvars_tidy, grid_sp_mapvars@data, by="id")

p <- ggplot() +
  geom_polygon(data = canada,
               aes(x=long, y=lat, group=group),
               fill = NA, color="black") + 
  geom_polygon(data=grid_sp_mapvars_tidy,
             aes(x=long, y=lat, group=group, 
                 color=dmspols_var_gwr,
                 fill=dmspols_var_gwr)) +
  theme_void() +
  scale_colour_viridis() +
  scale_fill_viridis() +
  labs(fill = "Coef on NTL",
       color = "Coef on NTL") 

ggsave(p, filename = file.path(figures_file_path, paste0("gwr_diff6_pos.png")), 
       height=7, width=8) 


