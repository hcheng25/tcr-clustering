packages <- c('tidyverse')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

dat <- read.csv('data/Europe_covid_data_cell_meta_data_w_stat_TRB-Pt-1.csv')

# ----- save TCR X column for later use -----
saveRDS(dat$X, 'rds/X.RDS')

# ----- convert frequencies into z-score by row -----
row_to_z <- function(df){
  col_names <- names(df)
  df <- as.data.frame(t(scale(t(df))))
  names(df) <- col_names
  return(df)
}

# ----- original counts for processing -----
counts <- dat|>
  select(starts_with('Count_'))

# ----- frequencies from original data -----
# log10(count/totals) except where count is 0
freq <- dat |>
  select(starts_with('Frequncy_')) |>
  # replace 0 with -7; equivalent to count relative frequency being 10^-7
  mutate(
    across(
      starts_with('Frequncy_'), \(x) ifelse(x==0, -7, x)
    )
  )
names(freq) <- gsub(pattern = 'Frequncy', replacement = 'Frequency', x = names(freq))

saveRDS(freq, 'rds/freq_neg7.RDS')
# note: this will also be used for plotting to check clusters visually

# ----- freq z-scores -----
freq_z <- freq |>
  row_to_z()

saveRDS(freq_z, 'rds/freq_z.RDS')

# ----- +1 pseudocount method -----
# log10((count+1)/row total)
plus1 <- counts |>
  mutate(across(starts_with('Count_'), \(x) x+10^-7), # add 10^-7 pseudocount
         row_total = rowSums(across(starts_with('Count_'))), # row sums
         across(starts_with('Count_'), \(x) log10(x/row_total)) # log of proportion of row total
  ) |>
  select(-row_total)
names(plus1) <- gsub(pattern = 'Count', replacement = 'Frequency', x = names(plus1))

saveRDS(plus1, 'rds/plus1.RDS')

# ----- log fold-change -----
# log(fold-change from previous timepoint)
counts_current <- counts |> # columns for timepoints 2+
  select(starts_with('Count_')) |>
  select(-1) |>
  mutate(across(starts_with('Count_'), \(x) x+1)) # pseudocount of 1 - prevent dividing by 0 at any point

counts_prev <- counts |> # columns starting at 1+, lines up so that each column is the one previous
  select(starts_with('Count_')) |>
  select(-last_col()) |>
  mutate(across(starts_with('Count_'), \(x) x+1)) # pseudocount of 1

log_foldchange <- (counts_current/counts_prev) |> # divide for fold change from timept to timept
  mutate(across(starts_with('Count_'), \(x) log(x))) |>
  rename_with(\(x) gsub(pattern='Count', 'Frequency', x=x), starts_with('Count'))
  # FOR FOLD CHANGE: dont use z score

saveRDS(log_foldchange, 'rds/log_foldchange.RDS')