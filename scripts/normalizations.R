packages <- c('tidyverse')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

dat <- read.csv('data/Europe_covid_data_cell_meta_data_w_stat_TRB-Pt-5.csv')
if(!dir.exists('rds')){
  dir.create('rds')
}

# ----- save TCR X column for later use -----
saveRDS(dat$X, 'rds/X.RDS')

# ----- convert frequencies into z-score by row -----
row_to_z <- function(df){
  col_names <- names(df)
  df <- as.data.frame(t(scale(t(df))))
  names(df) <- col_names
  return(df)
}

# ----- original counts for processing and later plotting -----
counts <- dat|>
  select(starts_with('Count_'))

saveRDS(counts, 'rds/counts.RDS')

# ----- frequencies from original data -----
# log10(count/totals) except where count is 0
freq <- dat |>
  select(starts_with('Frequncy_')) |>
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
# log10((count+1)/row total)
freq_plus_one_method <- counts |>
  mutate(across(starts_with('Count_'), \(x) x+10^-7), # add 10^-7 pseudocount
         row_total = rowSums(across(starts_with('Count_'))), # row sums
         across(starts_with('Count_'), \(x) log10(x/row_total)) # log of proportion of row total
  ) |>
  select(-row_total) |>
  row_to_z()
names(freq_plus_one_method) <- gsub(pattern = 'Count', replacement = 'Frequency', x = names(freq_plus_one_method))

saveRDS(freq_plus_one_method, 'rds/freq_plus_one_method.RDS')

# ----- log fold-change -----
# log(fold-change from previous timepoint)
counts_current <- counts |> # columns for timepoints 2+
  select(starts_with('Count_')) |>
  select(-1) |>
  mutate(across(starts_with('Count_'), \(x) x+10^-7))

counts_prev <- counts |> # columns starting at 1+, lines up so that each column is the one previous
  select(starts_with('Count_')) |>
  select(-last_col()) |>
  mutate(across(starts_with('Count_'), \(x) x+10^-7)) 

freq_log_foldchange <- (counts_current/counts_prev) |> # divide for fold change from timept to timept
  mutate(across(starts_with('Count_'), \(x) log(x))) |>
  rename_with(\(x) gsub(pattern='Count', 'Frequency', x=x), starts_with('Count'))
#  row_to_z() # FOR FOLD CHANGE: dont use z score

saveRDS(freq_log_foldchange, 'rds/freq_log_foldchange.RDS')
