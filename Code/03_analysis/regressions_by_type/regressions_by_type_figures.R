# Regressions by Code

country <- "mexico"

results_df <- readRDS(file.path(project_file_path, "Data", "Results", country, "reg_by_naics2.Rds"))
results_df$sig <- abs(results_df$`Pr(>|t|)`) <= 0.05

results_df$var_type_name <- results_df$var_type
results_df$var_type_name[results_df$var_type_name %in% "employment_sum"] <- "Employment"
results_df$var_type_name[results_df$var_type_name %in% "firms_sum"] <- "Firms"

results_df <- results_df 
results_df$var_transform %>% table()

results_df$diff <- results_df$var_transform %>%
  str_extract("diff(.)") %>%
  str_replace_all("diff", "") %>% 
  as.numeric() %>%
  as.factor()

results_df <- results_df[!is.na(results_df$naics2),]

# Levels -----------------------------------------------------------------------
for(grid_i in 25){
  
  p <- results_df %>%
    filter(grid %in% grid_i,
           var_type %in% c("firms_sum", "employment_sum"),
           var_transform %in% "") %>%
    ggplot(aes(x = reorder(naicsname, desc(naicsname)),
               y = Estimate,
               ymin = p025,
               ymax = p975)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_linerange() +
    geom_point() +
    coord_flip() +
    labs(x = "",
         color = "Year Difference") +
    theme_ipsum() +
    facet_wrap(~var_type_name,
               scales = "free_x")
    ggsave(p, filename = file.path(figures_file_path, 
                                paste0(country, 
                                       "_regbytype_",
                                       grid_i,
                                       "km",
                                       ".png")),
           height = 7, width = 15) 
  
  
}

# Difference -------------------------------------------------------------------
for(grid_i in 25){
  
  p <- results_df %>%
    filter(grid %in% grid_i,
           var_type %in% c("firms_sum", "employment_sum"),
           str_detect(var_transform, "log_diff(.)$")) %>%
    ggplot(aes(x = reorder(naicsname, desc(naicsname)),
               y = Estimate,
               ymin = p025,
               ymax = p975,
               color = diff,
               group = diff)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    geom_point(position = position_dodge(width = 0.7)) +
    coord_flip() +
    labs(x = "",
         color = "Year Difference") +
    scale_color_manual(values = wes_palette("Zissou1", 5, type = "continuous")) +
    theme_ipsum() +
    facet_wrap(~var_type_name,
               scales = "free_x") 
    ggsave(p, filename = file.path(figures_file_path, 
                                paste0(country, 
                                       "_regbytype_",
                                       grid_i,
                                       "km",
                                       "_diff.png")),
           height = 15, width = 15) 
  
  
}

