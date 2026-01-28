packages <- c('tidyverse')
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
  select(X, starts_with('Count_'))

# ----- frequencies from original data -----
# log10(count/totals) except where count is 0
freq <- dat |>
  select(X, starts_with('Frequncy_')) |>
  # replace 0 with -7; equivalent to count relative frequency being 10^-7
  mutate(
    across(
      starts_with('Frequncy_'), \(x) ifelse(x==0, -7, x)
    )
  )
freq <- cbind(freq[1], row_to_z(freq[-1]))

names(freq) <- gsub(pattern = 'Frequncy', replacement = 'Frequency', x = names(freq))
saveRDS(freq, 'rds/freq_neg7.RDS')

# ----- +1 method -----
# log10((count+1)/row total)
freq_plus_one_method <- counts |>
  mutate(across(starts_with('Count_'), \(x) x+10^-7), # add 10^-7 pseudocount
         row_total = rowSums(across(starts_with('Count_'))), # row sums
         across(starts_with('Count_'), \(x) log10(x/row_total)) # log of proportion of row total
  ) |>
  select(-row_total)
freq_plus_one_method <- cbind(freq_plus_one_method[1], row_to_z(freq_plus_one_method[-1]))

names(freq_plus_one_method) <- gsub(pattern = 'Count', replacement = 'Frequency', x = names(freq_plus_one_method))
saveRDS(freq_plus_one_method, 'rds/freq_plus_one_method.RDS')

# ----- log fold-change -----
# log((count+1)/(base_count+1))
freq_log_foldchange <- counts |>
  rowwise() |>
  mutate(across(starts_with('Count_'), \(x) x+10^-7), # add 10^-7 pseudocount
         across(starts_with('Count_'), \(x) x/Count_1)
         ) |>
  ungroup()


counts |>
  mutate(
    across(
      starts_with('Count_'), \(x) log((x+1)/(Count_1+1))
    )
  )
#  row_to_z() # FOR FOLD CHANGE: dont use z score
names(freq_log_foldchange) <- gsub(pattern = 'Count', replacement = 'Frequency', x = names(freq_log_foldchange))
saveRDS(freq_log_foldchange, 'rds/freq_log_foldchange.RDS')
