# Correlation Figure: Changes

# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization

country <- "Mexico"
firm_var_i <- "employment_sum_all"
ntl_var_i <- "VIIRS"

# Load/Prep Within Unit Correlation --------------------------------------------
cor_within <- readRDS(file.path(data_file_path, "Results", "correlation_within_unit.Rds"))
cor_within <- cor_within %>%
  dplyr::filter(!is.na(cor),
                #transform == "log",
                unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid"),
                ntl_var %in% c("dmspolszhang_mean", "viirs_mean")) %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolszhang_mean" ~ "DMSP-OLS",
                                    ntl_var %in% "viirs_mean"        ~ "VIIRS"),
                unit = unit %>% 
                  str_replace_all(" Grid", "") %>%
                  factor(levels = c("5km", "10km", "25km", "50km", "100km")))

cor_within %>%
  dplyr::filter(country %in% "Mexico",
                transform %in% "level",
                firm_var %in% "N_firms_sum_all",
                ntl_var %in% "DMSP-OLS") %>%
  ggplot(aes(x = cor, y = unit)) +
  geom_violin() 

cor_within %>%
  dplyr::filter(country %in% "Mexico",
                transform %in% "level",
                firm_var %in% "N_firms_sum_all",
                ntl_var %in% "DMSP-OLS") %>%
  ggplot(aes(x = cor)) +
  geom_histogram() +
  facet_wrap(~unit, 
             scale = "free_y",
             strip.position = "left",
             ncol = 1) +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  theme(strip.text = element_text(angle=0))

# Load/Prep Across Unit Correlation --------------------------------------------
cor_across <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

# Subset/Prep
cor_across <- cor_across %>%
  mutate(ntl_var = ntl_var %>% as.character,
         transform = transform %>% as.character) %>%
  filter(difference != "level",
         transform %in% c("log", "level"),
         ntl_var %in% c("dmspolszhang_mean", "viirs_mean"),
         !(year %in% "All"),
         unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")) %>%
  mutate(year = year %>% as.character() %>% as.numeric() %>% as.factor(),
         unit = unit %>% 
           str_replace_all(" Grid", "") %>%
           factor(levels = c("5km", "10km", "25km", "50km", "100km")),
         ntl_var = case_when(ntl_var %in% "dmspolszhang_mean" ~ "DMSP-OLS",
                             ntl_var %in% "viirs_mean"        ~ "VIIRS"),
         transform = case_when(transform %in% "log"   ~ "Logs",
                               transform %in% "level" ~ "Levels")) 

cor_across$firm_var[cor_across$firm_var %in% "empl_med_sum_all" & 
                      cor_across$year %in% c(2017, 2018, 2020) &
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


