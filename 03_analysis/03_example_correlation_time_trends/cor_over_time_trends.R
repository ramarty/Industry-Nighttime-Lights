# Analysis

set.seed(42)

# Load/Prep Data ---------------------------------------------------------------
mex_dmspols <- load_grid_data_no_type("mexico", "*_dmspols_clean.Rds", "all", "5km Grid")
mex_viirs   <- load_grid_data_no_type("mexico", "*_viirs_clean.Rds",   "all", "5km Grid")
can         <- load_grid_data_no_type("canada", "*_clean.Rds",         "all", "5km Grid")

# Correlation Dataframe --------------------------------------------------------
mex_viirs$employment_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)] <- 
  mex_viirs$empl_med_sum_all[mex_viirs$year %in% c(2017, 2018, 2020)]

make_cor_dmspols <- function(df){
  
  cor_df <- df %>%
    group_by(id) %>%
    dplyr::summarise(cor_ntl_firms  = cor(dmspolselvidge_mean_log, N_firms_sum_all_log),
                     cor_ntl_employ = cor(dmspolselvidge_mean_log,   employment_sum_all_log),
                     N_firms_sum_all_max = max(N_firms_sum_all),
                     employment_sum_all_max = max(employment_sum_all)) %>%
    filter(N_firms_sum_all_max > 0,
           employment_sum_all_max > 0) %>%
    arrange(runif(n())) 
  
  return(cor_df)
}

make_cor_viirs <- function(df){
  
  cor_df <- df %>%
    group_by(id) %>%
    dplyr::summarise(cor_ntl_firms  = cor(viirs_mean_log, N_firms_sum_all_log),
                     cor_ntl_employ = cor(viirs_mean_log, employment_sum_all_log),
                     N_firms_sum_all_max = max(N_firms_sum_all),
                     employment_sum_all_max = max(employment_sum_all)) %>%
    filter(N_firms_sum_all_max > 0,
           employment_sum_all_max > 0) %>%
    arrange(runif(n())) 
  
  return(cor_df)
}

mex_dmspols_df        <- make_cor_dmspols(mex_dmspols)
mex_dmspols_firms_id  <- mex_dmspols_df %>% filter(cor_ntl_firms > 0.9)  %>% pull(id) %>% head(10)
mex_dmspols_employ_id <- mex_dmspols_df %>% filter(cor_ntl_employ > 0.9) %>% pull(id) %>% head(10)

mex_viirs_df        <- make_cor_viirs(mex_viirs)
mex_viirs_firms_id  <- mex_viirs_df %>% filter(cor_ntl_firms > 0.8)  %>% pull(id) %>% head(10)
mex_viirs_employ_id <- mex_viirs_df %>% filter(cor_ntl_employ > 0.8) %>% pull(id) %>% head(10)

can_dmspols_df        <- make_cor_dmspols(can)
can_dmspols_firms_id  <- can_dmspols_df %>% filter(cor_ntl_firms > 0.9)  %>% pull(id) %>% head(10)
can_dmspols_employ_id <- can_dmspols_df %>% filter(cor_ntl_employ > 0.9) %>% pull(id) %>% head(10)

# Functions to Make Figures ----------------------------------------------------
make_figure <- function(ids, df, ntl_var, firm_var, title){
  p_all <- lapply(ids, make_figure_i, df, ntl_var, firm_var)
  
  p <- ggarrange(p_all[[1]],
                 p_all[[2]],
                 p_all[[3]],
                 p_all[[4]],
                 p_all[[5]],
                 p_all[[6]],
                 p_all[[7]],
                 p_all[[8]],
                 p_all[[9]],
                 p_all[[10]],
                 nrow = 2,
                 ncol = 5,
                 common.legend = T,
                 legend = "bottom") %>%
    annotate_figure(top = text_grob(title, color = "black", face = "bold", size = 14)
    )
  
  return(p)
}

make_figure_i <- function(id, df, ntl_var, firm_var){
  df_i <- df[df$id %in% id,] 
  
  if(grepl("firm", firm_var))   firm_title <- "N Firms"
  if(grepl("employ", firm_var)) firm_title <- "Employment"
  
  if(grepl("viirs", ntl_var))   sat_title <- "VIIRS"
  if(grepl("dmspo", ntl_var)) sat_title <- "DMSP-OLS"
  
  df_i$ntl_var <- df_i[[ntl_var]]
  df_i$firm_var <- df_i[[firm_var]]
  
  ntl_max <- df_i$ntl_var %>% max(na.rm=T)
  firm_max <- df_i$firm_var %>% max(na.rm=T)
  
  coef <- firm_max / ntl_max 
  if(coef < 1) coef <- 1
  
  df_i %>% 
    ggplot() +
    geom_line(aes(x = year, y = ntl_var, color = sat_title), size = 1.25) +
    geom_line(aes(x = year, y = firm_var/coef, color = firm_title), size = 0.75) +
    scale_color_manual(values = c("dodgerblue3", "darkorange2")) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . * coef,
                                           name = NULL)) +
    labs(x = NULL,
         y = NULL,
         color = NULL) +
    theme_minimal() +
    theme(axis.text.y.left = element_text(color = "darkorange2", face = "bold"),
          axis.text.y.right = element_text(color = "dodgerblue3", face = "bold"))
}

# Make Figures -----------------------------------------------------------------
p <- make_figure(can_dmspols_firms_id, can, "dmspolselvidge_mean", "N_firms_sum_all", "Canada, N Firms vs. DMSP-OLS")
ggsave(p, filename = file.path(figures_file_path, "can_dmspols_firms_cor_examples.png"), height = 5, width = 12)

p <- make_figure(can_dmspols_employ_id, can, "dmspolselvidge_mean", "employment_sum_all", "Canada, Employment vs. DMSP-OLS")
ggsave(p, filename = file.path(figures_file_path, "can_dmspols_employ_cor_examples.png"), height = 5, width = 12)


p <- make_figure(mex_dmspols_firms_id, mex_dmspols, "dmspolselvidge_mean", "N_firms_sum_all", "Mexico, N Firms vs. DMSP-OLS")
ggsave(p, filename = file.path(figures_file_path, "mex_dmspols_firms_cor_examples.png"), height = 5, width = 12)

p <- make_figure(mex_dmspols_employ_id, mex_dmspols, "dmspolselvidge_mean", "employment_sum_all", "Mexico, Employment vs. DMSP-OLS")
ggsave(p, filename = file.path(figures_file_path, "mex_dmspols_employ_cor_examples.png"), height = 5, width = 12)


p <- make_figure(mex_viirs_firms_id, mex_viirs, "viirs_mean", "N_firms_sum_all", "Mexico, N Firms vs. VIIRS")
ggsave(p, filename = file.path(figures_file_path, "mex_viirs_firms_cor_examples.png"), height = 5, width = 12)

p <- make_figure(mex_viirs_employ_id, mex_viirs, "viirs_mean", "employment_sum_all", "Mexico, Employment vs. VIIRS")
ggsave(p, filename = file.path(figures_file_path, "mex_viirs_employ_cor_examples.png"), height = 5, width = 12)
