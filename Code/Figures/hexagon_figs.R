# Extract DMSP-OLS

# Load Data --------------------------------------------------------------------
hexagon_data <- readRDS(file.path(final_data_file_path, "Canada Industry Data", "hexagon_data", "hexagons_data.Rds"))
hexagon_data <- hexagon_data[!is.na(hexagon_data$employment_mean),]

#### Round to nearest DMSP-OLS whole numbers
hexagon_data$dmspols_round <- hexagon_data$dmspols_mean %>% round()

#### Aggregate to DMSP Whole Number
hexagon_data_sum_df <- hexagon_data %>%
  group_by(dmspols_round) %>%
  summarise(N_mean = sum(N_sum, na.rm=T),
            employment_mean = mean(employment_mean, na.rm=T),
            employment_median = median(employment_mean, na.rm=T))

#### Figure
ggplot() +
  geom_point(data=hexagon_data_sum_df, aes(x=dmspols_round, y=employment_median))




 p <- ggplot() +
  geom_point(data=firmdata_sum_df, aes(x=dmspols_round, y=employment_mean), size=2) +
  stat_smooth(data=firmdata_df, aes(x=dmspols, employment)) +
  labs(x="Luminosity",
       y="Average\nEmployment") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = .5))
ggsave(p, filename = file.path(figures_file_path, "avg_employ_lum_trend.png"), height=3.5, width=5)



