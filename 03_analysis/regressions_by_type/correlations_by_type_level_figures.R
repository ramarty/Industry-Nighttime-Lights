# Regressions by Code

country <- "mexico"

# NAISC Code List --------------------------------------------------------------
naics_codes_df <- read_dta(file.path(project_file_path, "Data", "NAICS Codes", "naics2digit.dta"))
naics_codes_df$naicsname <- paste0(naics_codes_df$naics2, " - ", naics_codes_df$naicsname)
naics_codes_df <- naics_codes_df %>%
  dplyr::rename(naics_code_i = naics2) %>%
  mutate(naics_code_i = naics_code_i %>% as.character())

df <- readRDS(file.path(data_file_path, "Results", country, "polygon_correlation_results_bytype.Rds"))

df <- df %>%
  left_join(naics_codes_df, by = "naics_code_i")


df <- df[df$difference %in% "level",]
#df <- df[!(df$year %in% "All"),]
#df$year <- df$year %>% as.factor()

N_colors <- df$unit %>% unique() %>% length()

# Export -----------------------------------------------------------------------
p_list <- list()

i <- 1
for(firm_var in unique(df$firm_var)){
  
  if(firm_var %in% "firms_sum"){
    title <- "Number of Firms"
  }
  
  
  if(firm_var %in% "employment_sum"){
    title <- "Total Employment"
  }
  
  p_list[[i]] <- df[df$firm_var %in% firm_var,] %>%
    ggplot(aes(y = b,
               x = naicsname,
               color = unit,
               group =unit,
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
    scale_color_manual(values = wes_palette("Zissou1", N_colors, type = "continuous")) +
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
               ncol = 2)

p_title <- annotate_figure(p,
                           top = text_grob(country %>% tools::toTitleCase(), color = "black", face = "bold", size = 16) )

ggsave(p_title, filename = file.path(figures_file_path, paste0(country, "_level_coefs_firmemploy_VS_dmspol_mean_bytype.png")), height=5, width=15)


