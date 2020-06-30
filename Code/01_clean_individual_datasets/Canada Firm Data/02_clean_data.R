# Clean Firm Data

# Load Data --------------------------------------------------------------------
firms_df <- readRDS(file.path(data_file_path, "Canada Industry Data", "FinalData", "firms.Rds"))

# Restrict Variables to Only Needed Ones ---------------------------------------
# Large dataset, so helps with efficiency

firms_df@data <- firms_df@data %>%
  dplyr::select(year, employment, dmspols, naics2, naicsname)

# Remove Outliers --------------------------------------------------------------
firms_df$employment[(firms_df$naicsname %in% "Educational Services") &
                      firms_df$employment %in% 1000000] <- NA

# Fix naics --------------------------------------------------------------------
# https://en.wikipedia.org/wiki/North_American_Industry_Classification_System
firms_df$naics2[firms_df$naics2 %in% "41"] <- "42" # Wholesale Trade (41 in Canada,[3] 42 in the United States[2]);
firms_df$naicsname[firms_df$naics2 %in% "42"] <- "Wholesale Trade"

firms_df$naics2[firms_df$naics2 %in% "91"] <- "92" # (91 in the United States, 92 in Canada[4]);
firms_df$naicsname[firms_df$naics2 %in% "92"] <- "Public Administration"

# Export Data ------------------------------------------------------------------
saveRDS(firms_df, file.path(data_file_path, "Canada Industry Data", "FinalData", "firms_clean.Rds"))
