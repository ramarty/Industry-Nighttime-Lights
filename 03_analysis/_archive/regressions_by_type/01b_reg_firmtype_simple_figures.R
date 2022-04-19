# Regressions by Industry: Figures

# TODO: Don't do within changes for certain cases

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(data_file_path, "Results", "by_firm_type_ind_reg.Rds"))

# Prep Data --------------------------------------------------------------------
df <- df %>%
  mutate(ind_name = 
           case_when(ind_type == "agriculture" ~ "Agriculture",
                     ind_type == "bigindustry" ~ "Large Industry",
                     ind_type == "services" ~ "Services")) %>%
  mutate(ind_name = ind_name %>% as.factor() %>% fct_rev()) %>% 
  mutate(df_type = 
           case_when(df_type == "city" ~ "City",
                     df_type == "citygrid" ~ "Grid in Cities",
                     df_type == "hex_5km" ~ "5km Grid",
                     df_type == "hex_10km" ~ "10km Grid",
                     df_type == "hex_25km" ~ "25km Grid",
                     df_type == "hex_50km" ~ "50km Grid",
                     df_type == "hex_100km" ~ "100km Grid") %>%
           factor(levels = c("City",
                             "Grid in Cities",
                             "5km Grid",
                             "10km Grid",
                             "25km Grid",
                             "50km Grid",
                             "100km Grid"))) %>%
  mutate(type = 
           case_when(type == "id_FE" ~ "Panel [Unit FE]",
                     type == "year_FE" ~ "Panel [Year FE]",
                     type == "long_diff" ~ "Long Diff") %>%
           factor(levels = c("Panel [Year FE]",
                             "Panel [Unit FE]",
                             "Long Diff"))) %>%
  mutate(ntl_var = 
           case_when(ntl_var == "viirs_sum_log" ~ "VIIRS",
                     ntl_var == "dmspolsharmon_sum_log" ~ "DMSP-OLS")) %>%
  dplyr::filter(var != "(Intercept)") 

df <- df %>%
  dplyr::filter(control_other %in% F,
                var == "firm_var_i_log")

df <- df[!(df$country %in% "canada" & df$ntl_var %in% "VIIRS" & df$type %in% "Panel [Unit FE]"),]

# Function for Figure ----------------------------------------------------------
make_figure <- function(df, country_i, firm_var_i, ntl_var_i){
  
  df_sub <- df %>%
    dplyr::filter(country == country_i,
                  firm_var == firm_var_i) 
  
  N_colors <- df_sub$df_type %>% unique %>% length
  
  #if(grepl("viirs", ntl_var_i)) title <- "VIIRS"
  #if(grepl("dmsp", ntl_var_i))  title <- "DMSP-OLS"
  
  df_sub %>% 
    ggplot(aes(x = coef,
               xmin = ci2_5,
               xmax = ci97_5,
               y = df_type,
               group = ind_name,
               color = ind_name)) +
    geom_vline(xintercept = 0,
               alpha = 0.7) +
    geom_linerange(position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5)) +
    labs(color = "Firm Type",
         x = "Coef (+/- 95% CI)",
         y = NULL) +
    scale_color_manual(values = c("dodgerblue3", "orange", "black"),
                       guide = guide_legend(reverse = TRUE)) +
    theme_ipsum() +
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          strip.text = element_text(hjust = 0.5, face = "bold")) +
    facet_wrap(~ntl_var+type,
               scales = "free_x",
               nrow = 1) 
  
}

# Make Figures -----------------------------------------------------------------

## Canada - Firms
can_firm <- make_figure(df, "canada", "N_firms_sum") + ggtitle("Regression Results: Canada, Firms")
#ggsave(can_firm, filename = file.path(figures_file_path, "reg_firmtype_simpleols_can_firm.png"), height = HEIGHT, width=WIDTH)

## Canada - Employment
can_employ <- make_figure(df, "canada", "employment_sum") + ggtitle("Regression Results: Canada, Employment") 
#ggsave(can_employ, filename = file.path(figures_file_path, "reg_firmtype_simpleols_can_employ.png"), height = HEIGHT, width=WIDTH)

## Mexico - Firms
mex_firm <- make_figure(df, "mexico", "N_firms_sum") + ggtitle("Regression Results: Mexico, Employment")  
#ggsave(mex_firm, filename = file.path(figures_file_path, "reg_firmtype_simpleols_mex_firm.png"), height = HEIGHT, width=WIDTH)

p <- ggarrange(can_firm,
               can_employ,
               mex_firm,
               ncol=1,
               common.legend = T,
               legend = "bottom")
ggsave(p, filename = file.path(figures_file_path, "reg_firmtype_simpleols.png"), height = 16, width=13)


