# Analysis

# Load Data --------------------------------------------------------------------
grid <- readRDS(file.path(merged_data_grid, paste0("hex_10km_clean",".Rds")))

grid <- list.files(merged_data_grid, pattern = "*_clean.Rds", full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  filter(!is.na(unit)) %>%
  filter(!is.na(dmspol_mean)) %>%
  mutate(unit = unit %>% str_replace_all("hex_", ""))
grid$unit <- grid$unit 

grid <- grid[(grid$dmspol_mean > 0) | !is.na(grid$employment_mean_all),]
grid_non0 <- grid[(grid$dmspol_mean > 0) & !is.na(grid$employment_mean_all),]


unit <- "100km"
transform <- "log"
year <- 2001
dmspols_var <- "dmspol_mean"
firm_var <- "employment_sum_all"

df_out_all <- data.frame(NULL)

counter <- 1
for(year in c("2001", "2003", "2005", "2007", "2009", "2011", "2013", "All")){
  
  print(year)
  
  for(unit in unique(grid_non0$unit)){
    for(dmspols_var in c("dmspol_mean")){
      for(firm_var in c("employment_sum_all", "N_firms_sum_all")){
        for(transform in c("level", "log", "g5", "g25", "g50")){
          for(difference in c("level", paste0("diff",1:6))){
            
            #### Data Subset
            if(year %in% "All"){
              df_temp <- grid_non0[(grid_non0$unit %in% unit),]
            } else{
              year_i <- year %>% as.numeric
              df_temp <- grid_non0[(grid_non0$unit %in% unit) & (grid_non0$year %in% year_i),]
            }
            
            #### Transformation
            if(transform %in% "log_level"){
              dmspols_var_i <- paste0(dmspols_var, "_log")
              firm_var_i    <- firm_var
            } else if(transform %in% "level_log"){
              dmspols_var_i <- dmspols_var
              firm_var_i    <- paste0(firm_var, "_log")
            } else if(transform %in% "level"){
              dmspols_var_i <- dmspols_var
              firm_var_i    <- firm_var
            } else{
              dmspols_var_i <- paste0(dmspols_var, "_", transform)
              firm_var_i    <- paste0(firm_var, "_", transform)
            }
            
            #### Difference
            if(difference != "level"){
              dmspols_var_i <- paste0(dmspols_var_i, "_", difference)
              firm_var_i    <- paste0(firm_var_i, "_", difference)
            }
            
            df_temp$dmspols_var <- df_temp[[dmspols_var_i]]
            df_temp$firm_var    <- df_temp[[firm_var_i]]
            
            cor_out <- cor.test(df_temp$dmspols_var, df_temp$firm_var)
            
            ci <- cor_out$conf.int %>% as.numeric()
            df_out <- data.frame(b = cor_out$estimate,
                                 p = cor_out$p.value,
                                 ci_low = ci[1],
                                 ci_high = ci[2],
                                 unit = unit,
                                 transform = transform,
                                 dmspols_var = dmspols_var,
                                 firm_var = firm_var,
                                 difference = difference,
                                 year = year)
            
            df_out_all <- bind_rows(df_out, df_out_all)
          }
        }
      }
    }
  }
}

df_out_all$unit <- df_out_all$unit %>% factor(levels = c("5km", "10km", "25km", "50km", "100km", "250km", "500km", "1000km"))
df_out_all$year <- factor(df_out_all$year)

df_out_all <- df_out_all %>%
  mutate(transform = case_when(transform == "g50" ~ "50 Groups",
                               transform == "g25" ~ "25 Groups",
                               transform == "g5" ~ "5 Groups",
                               transform == "log" ~ "Logs",
                               transform == "level" ~ "Levels")) %>%
  mutate(transform = transform %>% factor(levels = c("Levels",
                                                     "Logs",
                                                     "5 Groups",
                                                     "25 Groups",
                                                     "50 Groups")))

saveRDS(df_out_all, file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

# Export -----------------------------------------------------------------------

p_list <- list()

i <- 1
for(firm_var in unique(df_out_all$firm_var)){
  
  if(firm_var %in% "N_firms_sum_all"){
    title <- "Number of Firms"
  }
  
  
  if(firm_var %in% "employment_sum_all"){
    title <- "Total Employment"
  }
  
  p_list[[i]] <- df_out_all[df_out_all$firm_var %in% firm_var,] %>%
    ggplot(aes(y = b,
               x = unit,
               color = year,
               group =year,
               ymin = ci_low,
               ymax = ci_high)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    geom_point(position = position_dodge(width = 0.7)) +
    labs(title = title,
         y = "Correlation Coefficient (+/- 95% CI)",
         x = "",
         color = "Year") +
    guides(color = guide_legend(reverse=T)) +
    #theme_minimal() +
    scale_color_manual(values = wes_palette("Zissou1", 7, type = "continuous")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~transform, 
               scales = "free_x",
               nrow=1) +
    coord_flip() 
  
  i <- i + 1
}

p <- ggarrange(p_list[[1]],
               p_list[[2]],
               common.legend = T,
               legend = "right",
               ncol = 1) 
ggsave(p, filename = file.path(figures_file_path, paste0("level_coefs_firmemploy_VS_dmspol_mean.png")), height=12, width=10)

