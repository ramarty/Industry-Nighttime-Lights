# Correlation Figure: Changes

country <- "Mexico"
firm_var_i <- "employment_sum_all"

# Load/Prep Data ---------------------------------------------------------------
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

# dmspols_mean, dmspolselvidge_mean, dmspolszhang_mean
df_out_all$ntl_var <- df_out_all$ntl_var %>% as.character()
df_out_all <- df_out_all[!(df_out_all$ntl_var %in% c("dmspols_mean", "dmspolselvidge_mean")),]
df_out_all$ntl_var[df_out_all$ntl_var %in% "dmspolszhang_mean"] <- "DMSP-OLS"
df_out_all$ntl_var[df_out_all$ntl_var %in% "viirs_mean"] <- "VIIRS"

df_out_all$transform <- df_out_all$transform %>% as.character()
df_out_all$transform[df_out_all$transform %in% "log"]   <- "Logs"
df_out_all$transform[df_out_all$transform %in% "level"] <- "Levels"

df_out_all <- df_out_all %>%
  filter(difference != "level",
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
make_figure <- function(firm_var_i, ntl_var_i, country, df_out_all){
  
  df_out_all <- df_out_all[df_out_all$country %in% country,]
  df_out_all <- df_out_all[df_out_all$firm_var %in% firm_var_i,]
  df_out_all <- df_out_all[df_out_all$ntl_var %in% ntl_var_i,]
  
  df_out_all <- df_out_all %>%
    filter(!is.na(b)) 
  df_out_all$diffyear <- paste(df_out_all$difference, df_out_all$year)
  N_colors <- df_out_all$difference %>% unique() %>% length()
  
  ## Prep Difference
  df_out_all$difference <- df_out_all$difference %>%
    str_replace_all("diff", "") %>% as.numeric()
  
  if(country %in% "Mexico" & ntl_var_i %in% "VIIRS")    df_out_all$difference <- df_out_all$difference * 1
  if(country %in% "Mexico" & ntl_var_i %in% "DMSP-OLS") df_out_all$difference <- df_out_all$difference * 5
  if(country %in% "Canada")                             df_out_all$difference <- df_out_all$difference * 2

  df_out_all$difference <- df_out_all$difference %>% as.factor()

  title <- country
  
  df_out_all %>%
    ggplot(aes(y = b,
               x = unit,
               color = difference,
               group = diffyear,
               ymin = ci_low,
               ymax = ci_high)) +
    geom_linerange(position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.8),
               pch = 21) +
    scale_fill_manual(values = c("white", "black")) +
    labs(title = title,
         y = "Correlation Coefficient (+/- 95% CI)",
         x = "",
         fill = "NTL Dataset",
         color = "Difference\n(Years)") +
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

# Make and Append Figures: Employment ------------------------------------------
p_viirs_employ_can <- make_figure("employment_sum_all", "VIIRS", "Canada", df_out_all)
p_dmsp_employ_can <- make_figure("employment_sum_all", "DMSP-OLS", "Canada", df_out_all)

p_viirs_employ_mex <- make_figure("employment_sum_all", "VIIRS", "Mexico", df_out_all)
p_dmsp_employ_mex <- make_figure("employment_sum_all", "DMSP-OLS", "Mexico", df_out_all)

p_viirs_employ <- ggarrange(p_viirs_employ_can,
                            p_viirs_employ_mex) %>%
  annotate_figure(top = text_grob("Correlation with Changes in VIIRS and Total Employment", 
                                  color = "black", face = "bold", size = 14))
ggsave(p_viirs_employ, filename = file.path(figures_file_path, "diff_cor_viirs_employment.png"), height = 8, width=15)

p_dmsp_employ <- ggarrange(p_dmsp_employ_can,
                           p_dmsp_employ_mex) %>%
  annotate_figure(top = text_grob("Correlation with Changes in DMSP-OLS and Total Employment", 
                                  color = "black", face = "bold", size = 14))
ggsave(p_dmsp_employ, filename = file.path(figures_file_path, "diff_cor_dmsp_employment.png"), height = 8, width=15)


# Make and Append Figures: Firms -----------------------------------------------
p_viirs_firms_can <- make_figure("N_firms_sum_all", "VIIRS", "Canada", df_out_all)
p_dmsp_firms_can <- make_figure("N_firms_sum_all", "DMSP-OLS", "Canada", df_out_all)

p_viirs_firms_mex <- make_figure("N_firms_sum_all", "VIIRS", "Mexico", df_out_all)
p_dmsp_firms_mex <- make_figure("N_firms_sum_all", "DMSP-OLS", "Mexico", df_out_all)

p_viirs_firms <- ggarrange(p_viirs_firms_can,
                           p_viirs_firms_mex) %>%
  annotate_figure(top = text_grob("Correlation with Changes in VIIRS and Total Firms", 
                                  color = "black", face = "bold", size = 14))
ggsave(p_viirs_firms, filename = file.path(figures_file_path, "diff_cor_viirs_firms.png"), height = 8, width=15)

p_dmsp_firms <- ggarrange(p_dmsp_firms_can,
                          p_dmsp_firms_mex) %>%
  annotate_figure(top = text_grob("Correlation with Changes in DMSP-OLS and Total Firms", 
                                  color = "black", face = "bold", size = 14))
ggsave(p_dmsp_firms, filename = file.path(figures_file_path, "diff_cor_dmsp_firms.png"), height = 8, width=15)


