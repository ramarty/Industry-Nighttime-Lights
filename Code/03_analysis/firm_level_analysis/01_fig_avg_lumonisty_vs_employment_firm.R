# Extract DMSP-OLS

country <- "mexico"
country_title <- country %>% tools::toTitleCase()

# Load Data --------------------------------------------------------------------
firmdata_df <- readRDS(file.path(project_file_path, "Data", 
                                 paste0(country_title, " Industry Data"), 
                                 "FinalData","firms_clean.Rds"))
firmdata_df <- firmdata_df@data
firmdata_df <- firmdata_df[!is.na(firmdata_df$employment),]

#### Round to nearest DMSP-OLS whole numbers
firmdata_df$dmspols_round <- firmdata_df$dmspols %>% round()

#### Remove Employment Outliers
firmdata_df$employment %>% quantile(0.9999)
firmdata_df <- firmdata_df[firmdata_df$employment <= 14000,]

#### Aggregate to DMSP Whole Number
firmdata_sum_df <- firmdata_df %>%
  group_by(dmspols_round) %>%
  summarise(employment_mean = mean(employment, na.rm=T),
            employment_median = median(employment, na.rm=T),
            employment_max = max(employment, na.rm=T))

#### Figure
 p <- ggplot() +
  geom_point(data=firmdata_sum_df, aes(x=dmspols_round, y=employment_mean), size=2) +
  stat_smooth(data=firmdata_df, aes(x=dmspols_round, employment)) +
  labs(x="Luminosity",
       y="Average\nEmployment") +
   labs(title = country_title ) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = .5),
        plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(p, filename = file.path(figures_file_path, 
                               paste0(country, "_avg_employ_lum_trend.png")), 
       height=3.5, width=5)



