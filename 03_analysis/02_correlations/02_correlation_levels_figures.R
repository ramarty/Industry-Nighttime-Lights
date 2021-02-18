# Correlation Figure: Levels

country <- "Mexico"
firm_var_i <- "employment_sum_all"

# Load/Prep Data ---------------------------------------------------------------
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

df_out_all$ntl_var <- df_out_all$ntl_var %>% as.character()
df_out_all$ntl_var[df_out_all$ntl_var %in% "dmspols_mean"] <- "DMSP-OLS"
df_out_all$ntl_var[df_out_all$ntl_var %in% "viirs_mean"] <- "VIIRS"

df_out_all$transform <- df_out_all$transform %>% as.character()
df_out_all$transform[df_out_all$transform %in% "log"]   <- "Logs"
df_out_all$transform[df_out_all$transform %in% "level"] <- "Levels"

df_out_all <- df_out_all %>%
  filter(difference %in% "level",
         ntl_var %in% c("DMSP-OLS", "VIIRS"),
         transform %in% c("Logs", "Levels"),
         !(year %in% "All")) %>%
  mutate(year = year %>% as.character() %>% as.numeric() %>% as.factor()) %>%
  mutate(unit = unit %>% 
           str_replace_all(" Grid", "") %>%
           factor(levels = c("5km", "10km", "25km", "50km", "100km", "250km", "500km", "1000km")))

df_out_all$firm_var[df_out_all$firm_var %in% "empl_med_sum_all" & 
                      df_out_all$year %in% c(2017, 2018, 2020) &
                      country %in% "Mexico"] <- "employment_sum_all"

# Make Figure Function ---------------------------------------------------------
make_figure <- function(firm_var_i, country, df_out_all){

  df_out_all <- df_out_all[df_out_all$country %in% country,]
  df_out_all <- df_out_all[df_out_all$firm_var %in% firm_var_i,]

  df_out_all <- df_out_all %>%
    filter(!is.na(b)) 
  N_colors <- df_out_all$year %>% unique() %>% length()
  
  #if(firm_var_i %in% "firms_sum_all")      title <- "Number of Firms"
  #if(firm_var_i %in% "employment_sum_all") title <- "Total Employment"
  title <- country
  
  df_out_all %>%
    ggplot(aes(y = b,
               x = unit,
               color = year,
               group =year,
               fill = ntl_var,
               ymin = ci_low,
               ymax = ci_high)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    geom_point(position = position_dodge(width = 0.7),
               pch = 21) +
    scale_fill_manual(values = c("white", "black")) +
    labs(title = title,
         y = "Correlation Coefficient (+/- 95% CI)",
         x = "",
         fill = "NTL Dataset",
         color = "Year") +
    guides(color = guide_legend(reverse=T),
           fill = guide_legend(reverse=T)) +
    #theme_minimal() +
    scale_color_manual(values = wes_palette("Zissou1", N_colors, type = "continuous")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~transform, 
               #scales = "free_x",
               nrow=1) +
    coord_flip() 
  
}

# Make and Append Figures ------------------------------------------------------
p_employ_mex <- make_figure("employment_sum_all", "Mexico", df_out_all)
p_employ_can <- make_figure("employment_sum_all", "Canada", df_out_all)

p_firms_mex <- make_figure("N_firms_sum_all", "Mexico", df_out_all)
p_firms_can <- make_figure("N_firms_sum_all", "Canada", df_out_all)

p_employ <- ggarrange(p_employ_can,
                      p_employ_mex) %>%
  annotate_figure(top = text_grob("Correlation with Nighttime Lights and Total Employment", 
                                  color = "black", face = "bold", size = 14))
ggsave(p_employ, filename = file.path(figures_file_path, "levels_cor_employment.png"), height = 6, width=15)

p_firms <- ggarrange(p_firms_can,
                     p_firms_mex)%>%
  annotate_figure(top = text_grob("Correlation with Nighttime Lights and Total Number of Firms", 
                                  color = "black", face = "bold", size = 14))
ggsave(p_firms, filename = file.path(figures_file_path, "levels_cor_firms.png"), height = 6, width=15)



