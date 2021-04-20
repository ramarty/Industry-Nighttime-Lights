# Correlation Figure: Changes

country <- "Canada"
firm_var_i <- "N_firms_sum_all"
ntl_var_i <- "DMSP-OLS"

# Load/Prep Across Unit Correlation --------------------------------------------
df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))

# Subset/Prep
df_out_all <- df_out_all %>%
  mutate(ntl_var = ntl_var %>% as.character,
         transform = transform %>% as.character) %>%
  filter(difference != "level",
         transform %in% c("log", "level"),
         ntl_var %in% c("dmspolsharmon_mean", "viirs_mean"),
         !(year %in% "All"),
         unit %in% c("5km Grid", "10km Grid", "25km Grid", "50km Grid", "100km Grid")) %>%
  mutate(year = year %>% as.character() %>% as.numeric() %>% as.factor(),
         unit = unit %>% 
           str_replace_all(" Grid", "") %>%
           factor(levels = c("5km", "10km", "25km", "50km", "100km")),
         ntl_var = case_when(ntl_var %in% "dmspolsharmon_mean" ~ "DMSP-OLS",
                             ntl_var %in% "viirs_mean"        ~ "VIIRS"))

df_out_all$firm_var[df_out_all$firm_var %in% "empl_med_sum_all" & 
                      df_out_all$year %in% c(2017, 2018, 2020) &
                      country %in% "Mexico"] <- "employment_sum_all"

# Make Figure Function ---------------------------------------------------------
make_figure <- function(firm_var_i, df_out_all){
  
  ## Prep
  df_out_all <- df_out_all %>%
    filter(#country %in% country_name,
           firm_var %in% firm_var_i,
           #ntl_var %in% ntl_var_i,
           transform %in% "log",
           !is.na(b)) %>%
    mutate(diffyear = paste(difference, year),
           difference = difference %>%
             str_replace_all("diff", "") %>% as.numeric)
  
  df_out_all$country_ntl <- paste0(df_out_all$country, ", ", df_out_all$ntl_var)
  
  N_colors <- df_out_all$difference %>% unique() %>% length()
  
  if(country %in% "Mexico" & ntl_var_i %in% "VIIRS")    df_out_all$difference <- df_out_all$difference * 1
  if(country %in% "Mexico" & ntl_var_i %in% "DMSP-OLS") df_out_all$difference <- df_out_all$difference * 5
  if(country %in% "Canada")                             df_out_all$difference <- df_out_all$difference * 2
  
  df_out_all$difference <- df_out_all$difference %>% as.factor()
  
  ## Figure
  if(firm_var_i %in% "employment_sum_all"){
    title <- "Correlation Between Nighttime Lights and Total Employment"
  } else if(firm_var_i %in% "N_firms_sum_all"){
    title <- "Correlation Between Nighttime Lights and Number of Firms"
  }

  df_out_all %>%
    ggplot(aes(y = b,
               x = unit,
               color = difference,
               group = diffyear,
               ymin = ci_low,
               ymax = ci_high)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
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
    coord_flip() +
    facet_wrap(~country_ntl,
               nrow = 1) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))
  
}

# Make Figures -----------------------------------------------------------------
p_employ <- make_figure("employment_sum_all", df_out_all)
ggsave(p_employ, filename = file.path(figures_file_path, "diff_cor_across_employ.png"), height = 6, width=12)

p_firm <- make_figure("N_firms_sum_all", df_out_all)
ggsave(p_firm, filename = file.path(figures_file_path, "diff_cor_across_firm.png"), height = 6, width=12)

