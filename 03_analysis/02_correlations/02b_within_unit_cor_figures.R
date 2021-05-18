# Correlation Figure: Changes Within Unit

# Load/Prep Within Unit Correlation --------------------------------------------
cor_within <- readRDS(file.path(data_file_path, "Results", "correlation_within_unit.Rds"))
cor_within <- cor_within %>%
  dplyr::filter(!is.na(cor),
                transform == "log",
                ntl_var %in% c("dmspolsharmon_sum", "viirs_sum")) %>%
  dplyr::mutate(ntl_var = case_when(ntl_var %in% "dmspolsharmon_sum" ~ "DMSP-OLS",
                                    ntl_var %in% "viirs_sum"        ~ "VIIRS"),
                firm_var = case_when(firm_var %in% "N_firms_sum_all"    ~ "N Firms",
                                     firm_var %in% "employment_sum_all" ~ "Employment")) %>%
  dplyr::mutate(unit = unit %>% factor(levels = rev(c("City",
                                                      "Grid in Cities",
                                                      "5km Grid",
                                                      "10km Grid",
                                                      "25km Grid",
                                                      "50km Grid",
                                                      "100km Grid"))))

# Function to Make Figure ------------------------------------------------------
make_figure <- function(country_name, ntl_var_name, firm_var_name){
  
  if(country_name %in% "Canada"){
    fill_color = "dodgerblue"
  } else{
    fill_color = "darkorange1"
  }
  
  title = paste0(country_name, "\n", ntl_var_name, " & ", firm_var_name)
  
  cor_within %>%
    dplyr::filter(country %in% country_name,
                  firm_var %in% firm_var_name,
                  ntl_var %in% ntl_var_name) %>%
    ggplot(aes(x = cor)) +
    geom_histogram(fill = fill_color,
                   color = "black") +
    labs(x = "Correlation",
         title = title) +
    theme(strip.background = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          strip.text = element_text(angle=0, size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~unit, 
               scale = "free_y",
               strip.position = "left",
               ncol = 1)
  
}

# Make Figures -----------------------------------------------------------------
can_dmsp_employ <- make_figure("Canada", "DMSP-OLS", "Employment")
can_dmsp_firms  <- make_figure("Canada", "DMSP-OLS", "N Firms")

mex_dmsp_firms  <- make_figure("Mexico", "DMSP-OLS", "N Firms")
mex_viirs_firms <- make_figure("Mexico", "VIIRS",    "N Firms")

# Append / Arrange Figures -----------------------------------------------------
p <- ggarrange(can_dmsp_employ,
               can_dmsp_firms,
               mex_dmsp_firms,
               mex_viirs_firms,
               nrow = 1) #%>%
#annotate_figure(top = text_grob("NTL and Firms", 
#                                color = "black", face = "bold", size = 14))

# Export -----------------------------------------------------------------------
ggsave(p, filename = file.path(figures_file_path, "diff_cor_within.png"), height = 8, width=12)




