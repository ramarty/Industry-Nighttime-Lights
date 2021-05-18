# Analysis

# Load/Prep Data ---------------------------------------------------------------
for(country_name in c("canada", "mexico")){
  
  ## Data
  viirs_df <- readRDS(file.path(data_file_path, "Grid", "FinalData", country_name, "merged_datasets", "citygridviirs.Rds"))
  dmsp_df  <- readRDS(file.path(data_file_path, "Grid", "FinalData", country_name, "merged_datasets", "citygriddmsp.Rds"))
  
  ## Template Rasters
  viirs_r <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS", 
                              paste0(country_name %>% substring(1,3),
                                     "_viirs_mean_",2013,".tif")))
  dmsp_r <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS_VIIRS_LI_HARMONIZED", 
                             paste0("Harmonized_DN_NTL_",2013,"_calDMSP_", 
                                    country_name %>% substring(1,3) %>% toupper(),
                                    ".tif")))
  
  viirs_df <- viirs_df %>%
    dplyr::mutate_at(vars(N_firms_sum_all,
                          employment_sum_all), replace_na, 0) 
  
  dmsp_df <- dmsp_df %>%
    dplyr::mutate_at(vars(N_firms_sum_all,
                          employment_sum_all), replace_na, 0) 
  
  if(country_name %in% "canada"){
    viirs_df <- viirs_df[viirs_df$year %in% 2013,]
    dmsp_df  <- dmsp_df[dmsp_df$year %in% 2013,]
  } else{
    viirs_df <- viirs_df[viirs_df$year %in% 2014,]
    dmsp_df  <- dmsp_df[dmsp_df$year %in% 2014,]
  }
  
  # Figures ----------------------------------------------------------------------
  cities <- viirs_df %>%
    group_by(city_name) %>%
    dplyr::summarise(N = n()) %>%
    arrange(-N) %>%
    head(7) %>%
    pull(city_name)
  
  p <- lapply(cities, function(city_name_i){
    
    #### Prep Rasters
    viirs_df <- viirs_df[viirs_df$city_name %in% city_name_i,]
    dmsp_df  <- dmsp_df[dmsp_df$city_name %in% city_name_i,]
    
    viirs_sp <- viirs_df
    coordinates(viirs_sp) <- ~lon+lat
    
    dmsp_sp <- dmsp_df
    coordinates(dmsp_sp) <- ~lon+lat
    
    viirs_r <- viirs_r %>% crop(viirs_sp)
    dmsp_r  <- dmsp_r %>% crop(dmsp_sp)
    
    r_data_firms <- rasterize(viirs_sp, viirs_r, field = "N_firms_sum_all")
    r_data_viirs <- rasterize(viirs_sp, viirs_r, field = "viirs_sum")
    r_data_dmsp <- rasterize(dmsp_sp, dmsp_r,   field = "dmspolsharmon_sum")
    
    r_data_firms_df <- r_data_firms %>% 
      coordinates() %>%
      data.frame()
    r_data_firms_df$value <- r_data_firms[]
    
    r_data_viirs_df <- r_data_viirs %>% 
      coordinates() %>%
      data.frame()
    r_data_viirs_df$value <- r_data_viirs[]
    
    r_data_dmsp_df <- r_data_dmsp %>% 
      coordinates() %>%
      data.frame()
    r_data_dmsp_df$value <- r_data_dmsp[]
    
    #### Name/Theme
    #city_name_i <- city_name_i %>% str_replace_all("-.*", "") %>% str_replace_all("/.*", "") %>% str_squish()
    city_name_i <- city_name_i %>% 
      str_replace_all("[[:digit:]]", "") %>% 
      str_squish() %>% 
      str_replace_all(" -$", "") %>%
      tolower() %>%
      tools::toTitleCase()
    
    if(country_name == "mexico"){
      # [City] - [District]; keep city
      city_name_i <- city_name_i %>% str_replace_all("-.*", "") %>% str_squish()
    }
    
    if(country_name == "canada"){
      # [District] - [City]; keep city
      city_name_i <- city_name_i %>% str_replace_all(".*-", "") %>% str_squish()
    }
    
    theme_custom <- theme(plot.title = element_text(hjust = 0.5,
                                                    face = "bold"),
                          legend.position = "none")
  
    #### Figures
    p_map_viirs <- r_data_viirs_df %>%
      ggplot() +
      geom_raster(aes(x = x, y = y, 
                    fill = value),
                size=1.5) +
      scale_fill_viridis(na.value = "white") +
      labs(color = "NTL",
           title = "VIIRS") +
      theme_void() +
      theme_custom
    
    p_map_dmsp <- r_data_dmsp_df %>%
      ggplot() +
      geom_raster(aes(x = x, y = y, 
                    fill = value)) +
      scale_fill_viridis(na.value = "white") +
      labs(color = "NTL",
           title = "DMSP-OLS") +
      theme_void() +
      theme_custom
    
    p_map_firms <- r_data_firms_df %>%
      ggplot() +
      geom_raster(aes(x = x, y = y, 
                    fill = log(value+1)),
                size = 1.5) +
      scale_fill_viridis(na.value = "white") +
      labs(color = "N Firms",
           title = "N Firms") +
      theme_void() +
      theme_custom
    
    p_cor_firms_viirs <- viirs_df %>%
      filter(employment_sum_all > 0) %>%
      ggplot() +
      geom_point(aes(x = log(N_firms_sum_all+1),
                     y = log(viirs_sum+1)),
                 size = 0.5,
                 alpha = 0.5) +
      labs(x = "N Firms (log scale)",
           y = "NTL (log scale)",
           title = "VIIRS & N Firms") +
      theme_minimal() +
      theme_custom
    
    p_cor_firms_dmsp <- dmsp_df %>%
      filter(employment_sum_all > 0) %>%
      ggplot() +
      geom_point(aes(x = log(N_firms_sum_all+1),
                     y = log(dmspolsharmon_sum+1)),
                 size = 0.5,
                 alpha = 0.5) +
      labs(x = "N Firms (log scale)",
           y = "NTL (log scale)",
           title = "DMSP & N Firms") +
      theme_minimal() +
      theme_custom
    
    ggarrange(p_map_firms, p_map_viirs, p_map_dmsp, p_cor_firms_viirs, p_cor_firms_dmsp, nrow = 1) %>%
      annotate_figure(
        top = text_grob(city_name_i, color = "black", face = "bold", size = 16),
      )
    
  })
  
  p_all <- ggarrange(p[[1]],
                     p[[2]],
                     p[[3]],
                     p[[4]],
                     p[[5]],
                     p[[6]],
                     p[[7]],
                     nrow = 7)
  
  ggsave(p_all, 
         filename = file.path(figures_file_path, paste0("within_city_maps_cor_", country_name, ".png")),
         height = 16, 
         width = 12)
  
}
