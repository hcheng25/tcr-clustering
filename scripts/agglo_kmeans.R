packages <- c('cluster', 'ggplot2', 'rlang', 'Hmisc', 'tidyverse')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/agglo_kmeans')){
  dir.create('results/agglo_kmeans')
  dir.create('results/agglo_kmeans/freq_plots')
  dir.create('results/agglo_kmeans/check_plots')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# ----- using gap statistic to select number of clusters -----
# col_kmeans
# input: numeric vector i.e. one frequency column
# output: kmeans cluster assignments for that frequency column based on best gap statistics
col_kmeans <- function(column, B=50){
  # use gap statistic to select number of clusters
  column <- as.data.frame(column) # ensure column vector is a data frame for clusgap
  gap_stat <- clusGap(x = column,
                      FUN = kmeans,
                      K.max = 15,
                      nstart = 25,
                      iter.max = 50,
                      B = B
                      )
  optimal_k <- maxSE(gap_stat$Tab[, "gap"],
                     gap_stat$Tab[, "SE.sim"],
                     method = "firstSEmax")
  
  km <- kmeans(column, centers = optimal_k, nstart = 25, iter.max=50)
  return(km$cluster)
}

# ----- flatten correlation matrix -----
# looks at r and p for combinations of agg_cluster
flattenCorrMatrix <- function(rmat, pmat){
  ut <- upper.tri(rmat) # use only upper triangle of correlation matrix
  flattened <- data.frame(row = rownames(rmat)[row(rmat)[ut]],
                          column = rownames(rmat)[col(rmat)[ut]],
                          r = rmat[ut],
                          p = pmat[ut]
                          )
  return(flattened)
}

# ----- for testing -----
test <- all_sets$freq_z |>
  mutate(across(starts_with('Frequency_'), col_kmeans, .names='{.col}_cluster')) %>%
  mutate(agg_cluster = apply(X = select(., ends_with('_cluster')),
                             MARGIN = 1,
                             FUN = paste0,
                             collapse = '_')) |>
  select(-(starts_with('Frequency_') & ends_with('_cluster')))

test_meds <- test |>
  group_by(agg_cluster) |>
  summarise(across(starts_with('Frequency_'), \(x) median(x, na.rm=TRUE))) |>
  column_to_rownames('agg_cluster')

test_matrix <- rcorr(t(test_meds)) # want to compute correlation across clusters, which are originally in rows

flattened <- flattenCorrMatrix(test_matrix$r, test_matrix$P) |>
  filter(r>0.9, p<0.05) # select those that are highly correlated with signficant p


# PICK UPHERE


# ii <- 1
# gap_stat_kmeans(df = all_sets[[ii]],
#                 df_name = names(all_sets[ii]),
#                 y_lab = y_lab[ii],
#                 B = 5) # adjusted to lower number for testing function

# ----- actual run -----
for (ii in seq_along(all_sets)){
  gap_stat_kmeans(df = all_sets[[ii]],
                  df_name = names(all_sets[ii]),
                  y_lab = y_lab[ii],
                  B = 50) 
}
