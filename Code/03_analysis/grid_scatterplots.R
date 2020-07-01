# Analysis

country <- "mexico"
country_title <- country %>% tools::toTitleCase()

# Load Data --------------------------------------------------------------------
readRDS_excludevars <- function(filepath){
  # Exclude variables by category
  df <- readRDS(filepath)
  rm_vars <- names(df)[str_detect(names(df), "t([[:digit:]])([[:digit:]])")]
  
  df <- df %>%
    dplyr::select(-all_of(rm_vars))
  
  return(df)
}

grid <- list.files(file.path(project_file_path, "Data", 
                             "Grid",
                             "FinalData",
                             country,
                             "merged_datasets"), pattern = "*_clean.Rds", full.names = T) %>%
  lapply(readRDS_excludevars) %>%
  bind_rows() %>%
  filter(!is.na(unit)) %>%
  filter(!is.na(dmspol_mean)) %>%
  mutate(unit = unit %>% str_replace_all("hex_", ""))
grid$unit <- grid$unit %>% factor(levels = c("5km", "10km", "25km", "50km", "100km", "250km", "500km", "1000km"))

grid <- grid[(grid$dmspol_mean > 0) | !is.na(grid$employment_sum_all),]
grid_non0 <- grid[(grid$dmspol_mean > 0) & !is.na(grid$employment_sum_all),]

p <- ggplot(data = grid_non0, 
       aes(x = dmspol_mean_log, y = employment_sum_all_log)) +
  geom_point(size=.1) +
  labs(x="Log(Nighttime Lights)",
       y="Log(Total Employment)",
       title = country_title) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) + 
  facet_wrap(~unit,
             nrow = 2,
             scales = "free") 
ggsave(p, filename = file.path(figures_file_path, paste0(country, "_employlog_ntllog_scatter.png")), height=6, width=10)





