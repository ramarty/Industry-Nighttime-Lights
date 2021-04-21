# Correlation Figure: Levels

country <- "Mexico"
firm_var_i <- "employment_sum_all"

# Load/Prep Data ---------------------------------------------------------------
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

## Rename
df_out_all$ntl_var <- df_out_all$ntl_var %>% as.character()
df_out_all$ntl_var[df_out_all$ntl_var %in% "dmspols_mean"] <- "DMSP-OLS"
df_out_all$ntl_var[df_out_all$ntl_var %in% "viirs_mean"] <- "VIIRS"

df_out_all$transform <- df_out_all$transform %>% as.character()
df_out_all$transform[df_out_all$transform %in% "level"] <- "Levels"

## Subset/Prep Variables
df_out_all <- df_out_all %>%
  filter(difference %in% "level",
         ntl_var %in% c("DMSP-OLS", "VIIRS"),
         transform %in% c("Levels"),
         !(year %in% "All"),
         unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")) %>%
  mutate(year = year %>% as.character() %>% as.numeric() %>% as.factor()) %>%
  mutate(unit = unit %>% 
           str_replace_all(" Grid", "") %>%
           factor(levels = c("5km", "10km", "25km", "50km", "100km")))

df_out_all$firm_var[df_out_all$firm_var %in% "empl_med_sum_all" & 
                      df_out_all$year %in% c(2017, 2018, 2020) &
                      country %in% "Mexico"] <- "employment_sum_all"

df_out_all <- df_out_all %>%
  dplyr::filter(firm_var %in% c("N_firms_sum_all", "employment_sum_all")) %>%
  dplyr::mutate(firm_var =
                  case_when(firm_var %in% "N_firms_sum_all" ~ "N Firms",
                            firm_var %in% "employment_sum_all" ~ "Employment"))

# Make Figure Function ---------------------------------------------------------
make_figure <- function(country, df_out_all){
  
  df_out_all <- df_out_all[df_out_all$country %in% country,]
  #df_out_all <- df_out_all[df_out_all$firm_var %in% firm_var_i,]
  
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
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    geom_point(position = position_dodge(width = 0.7),
               pch = 21) +
    ylim(c(0,1)) +
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
    facet_wrap(~firm_var, 
               #scales = "free_x",
               nrow=1) +
    coord_flip() 
  
}

# Make and Append Figures ------------------------------------------------------
p_mex <- make_figure("Mexico", df_out_all)
p_can <- make_figure("Canada", df_out_all)

p <- ggarrange(p_mex,
               p_can) 
ggsave(p, filename = file.path(figures_file_path, "levels_cor.png"), height = 5, width=15)


# p_employ <- ggarrange(p_mex,
#                       p_can) %>%
#   annotate_figure(top = text_grob("Correlation with Nighttime Lights and Total Employment", 
#                                   color = "black", face = "bold", size = 14))
# ggsave(p_employ, filename = file.path(figures_file_path, "levels_cor_employment.png"), height = 5, width=15)


