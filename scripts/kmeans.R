packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/kmeans')){
  dir.create('results/kmeans')
}
if(!dir.exists('results/kmeans/freq_plots')){
  dir.create('results/kmeans/freq_plots')
}
if(!dir.exists('results/kmeans/log10_plots')){
  dir.create('results/kmeans/log10_plots')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# ----- using gap statistic to select number of clusters -----
# counts for plotting the non normalized plots based on cluster assignments
gap_stat_kmeans <- function(df, df_name, y_lab, log10counts, B=50){
  test_frame <- df

  # use gap statistic to select number of clusters
  gap_stat <- clusGap(x = test_frame,
                      FUN = kmeans,
                      K.max = 35,
                      nstart = 25,
                      B = B
                      )
  # print(gap_stat, method='Tibs2001SEmax')
  optimal_k <- maxSE(gap_stat$Tab[, "gap"],
                     gap_stat$Tab[, "SE.sim"],
                     method = "firstSEmax")
  # plot(gap_stat)
  # note: trouble with convergence
  
  km <- kmeans(test_frame, centers = optimal_k, nstart = 25)
  set_with_clusters <- test_frame
  set_with_clusters$cluster <- as.factor(km$cluster)
  for (jj in seq_along(levels(set_with_clusters$cluster))){
    levels(set_with_clusters$cluster)[jj] <- paste0('Cluster_',
                                                    levels(set_with_clusters$cluster)[jj],
                                                    ' (n=',
                                                    table(set_with_clusters$cluster)[[jj]],
                                                    ')')
  }
  set_with_clusters$X <- as.factor(X)
  
  # plot clusters to examine z-score trends in each cluster
  freq_long <- set_with_clusters |>
    pivot_longer(cols = starts_with('Frequency_'),
                 names_to = 'Timepoint',
                 values_to = 'Frequency') |>
    mutate(
      Timepoint = factor(gsub(pattern='Frequency_', replacement='', Timepoint), levels=1:9)
    )
  
  p <- ggplot(freq_long, aes(x = Timepoint, y = Frequency, group = X, color = X)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~ cluster) +
    theme(legend.position = 'none') + 
    labs(title = paste0('Cluster plots for normalized frequency (', df_name, ')'),
         x = 'Timepoint',
         y = y_lab
    )
  p
  
  plot_save_path <- paste0('results/kmeans/freq_plots/', df_name, '_cluster_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='px', width=3000, height=5000) # save cluster plots
  
  # assign clusters to raw counts for plotting counts
  log10counts$cluster <- as.factor(km$cluster)
  for (jj in seq_along(levels(log10counts$cluster))){
    levels(log10counts$cluster)[jj] <- paste0('Cluster_',
                                         levels(log10counts$cluster)[jj],
                                         ' (n=',
                                         table(log10counts$cluster)[[jj]],
                                         ')')
  }
  log10counts$X <- as.factor(X)
  
  # plot clusters to examine raw count trends in each cluster
  freq_long <- log10counts |>
    pivot_longer(cols = starts_with('Count_'),
                 names_to = 'Timepoint',
                 values_to = 'Count') |>
    mutate(
      Timepoint = factor(gsub(pattern='Count_', replacement='', Timepoint), levels=1:9)
    )
  
  p <- ggplot(freq_long, aes(x = Timepoint, y = Count, group = X, color = X)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~ cluster,
               ncol = 3,
               scales = 'free_y') +
    theme(legend.position = 'none') + 
    labs(title = paste0('Cluster plots for log10(counts) of ', df_name),
         x = 'Timepoint',
         y = 'Log10 of Raw Count'
    )
  p
  
  plot_save_path <- paste0('results/kmeans/log10_plots/', df_name, '_log10_cluster_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='px', width=3000, height=5000) # save cluster plots
}

for (ii in seq_along(all_sets)){
  
  if(ii==3){ # for freq_log_foldchange
    y_lab <- 'Log Fold-change from Previous'
  }else if(ii %in% c(1,2)){ # freq, freq+1method
    y_lab <- 'Normalized Frequency (z-score)'
  }else{
    y_lab <- '(Unspecified y label)'
  }
  
  gap_stat_kmeans(df = all_sets[[ii]],
                  df_name = names(all_sets[ii]),
                  y_lab = y_lab,
                  log10counts = log10counts,
                  B = 50) # adjust to lower number if just testing code 
}
