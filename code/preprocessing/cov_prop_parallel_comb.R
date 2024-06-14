library(future)
library(future.apply)
library(data.table)
library(dplyr)

# Combine all cov_prop outputs into one dataframe

plan(multisession)

# Load lot info
lot_info <- do.call(
  rbind,
  future_lapply(list.files("output/lot_info", full.names=T), fread)
)

slats_prop_output_dir <- "output/slats_prop"
slats_prop_df <- do.call(
  rbind,
  future_lapply(list.files(slats_prop_output_dir, full.names=T), fread)
)

slats_prop_df[, year := as.numeric(substr(slats_prop_df[,lyr], 7, 10))]
slats_prop_df[, end_year := as.numeric(substr(slats_prop_df[,lyr], 12, 15))]

fwrite(slats_prop_df, "output/model_data/slats_prop_df.csv")

## Combine outputs from cov_prop
cov_prop_output_dir <- "output/cov_prop_parallel"
reshape_df <- function(path) {
  df <- fread(path) %>%
    select(-prec, -temp)
  df_prec <- df %>%
    pivot_longer(cols = starts_with("prec_"), names_to = 'year', names_prefix = 'prec_',
                 values_to = 'prec') %>%
    select(lot_id, year, prec)
  df_temp <- df %>%
    pivot_longer(cols = starts_with("temp_"), names_to = 'year', names_prefix = 'temp_',
                 values_to = 'temp') %>%
    select(lot_id, year, temp)
  df_woody <- df %>%
    pivot_longer(cols = starts_with("woody_veg_"), names_to = 'year', names_prefix = 'woody_veg_',
                 values_to = 'woody_veg') %>%
    select(lot_id, year, woody_veg)
  df_comb <- reduce(list(df_prec, df_temp, df_woody), left_join, by = c('lot_id', 'year'))
  
  df_out <- df %>%
    select(-starts_with("prec"), -starts_with("temp"), -starts_with("woody_veg")) %>%
    right_join(df_comb, by = 'lot_id')
  return(df_out)
}

file_list <- list.files(cov_prop_output_dir, full.names = T)
file_id <- gsub("\\D", "", list.files(cov_prop_output_dir, full.names = F)) %>% as.numeric()
file_list_subset <- file_list[file_id %% 1 == 0] # Subset a sample of the data if needed
cov_prop_comb <- future_lapply(file_list_subset, reshape_df)
cov_prop_comb <- do.call(rbind, cov_prop_comb)

fwrite(cov_prop_comb, "output/model_data/cov_prop_comb.csv")