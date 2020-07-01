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
results_df <- results_df[!(results_df$naics2 %in% "NA"),]

results_df <- results_df[!is.na(results_df$naicsname),]
results_df <- results_df[!(results_df$naicsname %in% "NA"),]



# Levels -----------------------------------------------------------------------
p <- results_df %>%
  filter(dv %in% "dmspols_sum_all",
         difference %in% "",
         var_type %in% c("firms_sum", "employment_sum"),
         var_transform %in% "_log") %>% # _log
  ggplot(aes(x = reorder(naicsname, desc(naicsname)),
             y = Estimate,
             ymin = p025,
             ymax = p975,
             group = grid %>% as.factor(),
             color = grid %>% as.factor())) +
  geom_hline(yintercept = 0, alpha = 0.1) +
  geom_linerange(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7)) +
  guides(color = guide_legend(reverse=T)) +
  scale_color_manual(values = wes_palette("Zissou1", 7, type = "continuous")) +
  coord_flip() +
  labs(x = "",
       color = "Grid Size",
       title = country %>% tools::toTitleCase()) +
  theme_ipsum() +
  facet_wrap(~var_type_name,
             scales = "free_x")
p

ggsave(p, filename = file.path(figures_file_path, 
                               paste0(country, 
                                      "_regbytype",
                                      ".png")),
       height = 10, width = 15) 




# Difference -------------------------------------------------------------------
for(grid_i in 25){
  
  if(country %in% "mexico"){
    N_colors <- 2
    
    results_df$difference[results_df$difference %in% "_diff1"] <- "2 Year Change"
    results_df$difference[results_df$difference %in% "_diff2"] <- "4 Year Change"
    
  }   
  
  if(country %in% "canada"){
    N_colors <- 5
    
    results_df$difference[results_df$difference %in% "_diff1"] <- "2 Year Change"
    results_df$difference[results_df$difference %in% "_diff2"] <- "4 Year Change"
    results_df$difference[results_df$difference %in% "_diff3"] <- "6 Year Change"
    results_df$difference[results_df$difference %in% "_diff4"] <- "8 Year Change"
    results_df$difference[results_df$difference %in% "_diff5"] <- "10 Year Change"
    results_df$difference[results_df$difference %in% "_diff6"] <- "12 Year Change"
    
    results_df$difference <- factor(results_df$difference, levels = c("2 Year Change",
                                                                         "4 Year Change",
                                                                         "6 Year Change",
                                                                         "8 Year Change",
                                                                         "10 Year Change",
                                                                         "12 Year Change"))
    
  }   
  
  p <- results_df %>%
    filter(dv %in% "dmspols_sum_all",
           grid %in% grid_i,
           var_type %in% c("firms_sum", "employment_sum"),
           var_transform %in% "",
           difference != "") %>%
    ggplot(aes(x = reorder(naicsname, desc(naicsname)),
               y = Estimate,
               ymin = p025,
               ymax = p975,
               color = difference,
               group = difference)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    geom_point(position = position_dodge(width = 0.7)) +
    coord_flip() +
    guides(color = guide_legend(reverse=T)) +
    labs(x = "",
         color = "Year Difference",
         title = country %>% tools::toTitleCase()) +
    scale_color_manual(values = wes_palette("Zissou1", N_colors, type = "continuous")) +
    theme_ipsum() +
    facet_wrap(~var_type_name,
               scales = "free_x") 
  ggsave(p, filename = file.path(figures_file_path, 
                                 paste0(country, 
                                        "_regbytype_",
                                        grid_i,
                                        "km",
                                        "_diff.png")),
         height = 13, width = 15) 
  
  
}

