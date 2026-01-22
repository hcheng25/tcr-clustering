packages <- c('tidyverse')
lapply(packages, install.packages)
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

dat <- read.csv('data/Europe_covid_data_cell_meta_data_w_stat_TRB-Pt-1.csv')

# ----- convert frequencies into z-score by row -----
row_to_z <- function(df){
  col_names <- names(df)
  df <- as.data.frame(t(scale(t(df))))
  names(df) <- col_names
  return(df)
}

# ----- original counts for processing -----
counts <- dat|>
  select(X, paste0('Count_', 1:9))

# ----- frequencies from original data -----
# log10(count/totals) except where count is 0
freq <- dat |>
  select(X, paste0('Frequncy_', 1:9)) |>
  # replace 0 with -7; equivalent to count relative frequency being 10^-7
  mutate(
    across(
      starts_with('Frequncy_'), \(x) ifelse(x==0, -7, x)
    )
  ) |>
  row_to_z()
names(freq) <- gsub(pattern = 'Frequncy', replacement = 'Frequency', x = names(freq))
saveRDS(freq, 'rds/freq_neg7.RDS')

# ----- +1 method -----
# log10((count+1)/total)
freq_plus_one_method <- counts |>
  mutate(
    across(
      starts_with('Count_'), \(x) log10((x+1)/sum(x+1)) # add 1 to avoid log(0)
    )
  ) |>
  row_to_z()
names(freq_plus_one_method) <- gsub(pattern = 'Count', replacement = 'Frequency', x = names(freq_plus_one_method))
saveRDS(freq_plus_one_method, 'rds/freq_plus_one_method.RDS')

# ----- log fold-change -----
# log((count+1)/(base_count+1))
freq_log_foldchange <- counts |>
  mutate(
    across(
      starts_with('Count_'), \(x) log((x+1)/(Count_1+1))
    )
  ) |>
  row_to_z()
names(freq_log_foldchange) <- gsub(pattern = 'Count', replacement = 'Frequency', x = names(freq_log_foldchange))
saveRDS(freq_log_foldchange, 'rds/freq_log_foldchange.RDS')
