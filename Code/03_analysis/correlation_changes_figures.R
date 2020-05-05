

df_out_all <- readRDS(file.path(data_file_path, "Results", "polygon_correlation_results.Rds"))
df_out_all <- df_out_all[df_out_all$difference != "level",]
df_out_all <- df_out_all[(df_out_all$year %in% "All"),]

df_out_all <- df_out_all %>%
  
  mutate(difference = case_when(difference == "diff1" ~ "2 Year Change",
                                difference == "diff2" ~ "4 Year Change",
                                difference == "diff3" ~ "6 Year Change",
                                difference == "diff4" ~ "8 Year Change",
                                difference == "diff5" ~ "10 Year Change",
                                difference == "diff6" ~ "12 Year Change")) %>%
  mutate(difference = difference %>% factor(levels = c("2 Year Change",
                                                      "4 Year Change",
                                                      "6 Year Change",
                                                      "8 Year Change",
                                                      "10 Year Change",
                                                      "12 Year Change")))

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
               color = difference,
               group =difference,
               ymin = ci_low,
               ymax = ci_high)) +
    geom_hline(yintercept = 0, size=.2, color="black", alpha=0.5) +
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
ggsave(p, filename = file.path(figures_file_path, paste0("difference_coefs_firmemploy_VS_dmspol_mean.png")), height=12, width=10)

