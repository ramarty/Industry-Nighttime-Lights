# Correlation Figure: Changes Within Unit

# FIRMS
# Mexico [DMSPOLS] <--> Canada [DMSPOLS]
# Within | Across  <--> Within | Across

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
                ntl_var %in% c("dmspolsharmon_mean", "viirs_mean")) %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolsharmon_mean" ~ "DMSP-OLS",
                                    ntl_var %in% "viirs_mean"        ~ "VIIRS"),
                unit = unit %>% 
                  str_replace_all(" Grid", "") %>%
                  factor(levels = rev(c("5km", "10km", "25km", "50km", "100km"))))

# Function to Make Figure ------------------------------------------------------
make_figure <- function(country_name, ntl_var_name, firm_var_name){
  
  if(firm_var_name %in% "N_firms_sum_all"){
    fill_color = "dodgerblue"
  } else{
    fill_color = "darkorange1"
  }
  
  cor_within %>%
    dplyr::filter(country %in% country_name,
                  transform %in% "log",
                  firm_var %in% firm_var_name,
                  ntl_var %in% ntl_var_name) %>%
    ggplot(aes(x = cor)) +
    geom_histogram(fill = fill_color,
                   color = "black") +
    labs(x = "Correlation",
         title = paste0(country_name, ", ", ntl_var_name)) +
    theme(strip.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          strip.text = element_text(angle=0),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~unit, 
               scale = "free_y",
               strip.position = "left",
               ncol = 1)
  
}

# Make Figures -----------------------------------------------------------------

can_dmsp_firms  <- make_figure("Canada", "DMSP-OLS", "N_firms_sum_all")
mex_dmsp_firms  <- make_figure("Mexico", "DMSP-OLS", "N_firms_sum_all")
mex_viirs_firms <- make_figure("Mexico", "VIIRS",    "N_firms_sum_all")

can_dmsp_employ  <- make_figure("Canada", "DMSP-OLS", "employment_sum_all")
mex_dmsp_employ  <- make_figure("Mexico", "DMSP-OLS", "employment_sum_all")
mex_viirs_employ <- make_figure("Mexico", "VIIRS",    "employment_sum_all")

# Append / Arrange Figures -----------------------------------------------------
firms <- ggarrange(can_dmsp_firms,
                   mex_dmsp_firms,
                   mex_viirs_firms,
                   nrow = 1) %>%
  annotate_figure(top = text_grob("NTL and Firms", 
                                  color = "black", face = "bold", size = 14))

employment <- ggarrange(can_dmsp_employ,
                        mex_dmsp_employ,
                        mex_viirs_employ,
                        nrow = 1) %>%
  annotate_figure(top = text_grob("NTL and Employment", 
                                  color = "black", face = "bold", size = 14))

p <- ggarrange(firms, employment,
          nrow = 1) %>%
  annotate_figure(top = text_grob("Distribution of Within Unit Correlation between NTL and Firm Outcomes", 
                                  color = "black", face = "bold", size = 14))

# Export -----------------------------------------------------------------------
ggsave(p, filename = file.path(figures_file_path, "diff_cor_within.png"), height = 8, width=15)




