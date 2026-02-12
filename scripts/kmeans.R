packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/kmeans')){
  dir.create('results/kmeans')
  dir.create('results/kmeans/freq_plots')
  dir.create('results/kmeans/check_plots')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# ----- using gap statistic to select number of clusters -----
# counts for plotting the non normalized plots based on cluster assignments
gap_stat_kmeans <- function(df, df_name, y_lab, B=50){
  # use gap statistic to select number of clusters
  gap_stat <- clusGap(x = df,
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
  
  km <- kmeans(df, centers = optimal_k, nstart = 25)
  set_with_clusters <- df
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
    facet_wrap(~ cluster,
               ncol = 3,
               scales = 'free_y') +
    theme(legend.position = 'none') + 
    labs(title = paste0('K-Means Cluster Plots (', df_name, ')'),
         x = 'Timepoint',
         y = y_lab
    )
  p
  
  plot_save_path <- paste0('results/kmeans/freq_plots/', df_name, '_cluster_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='in', width=4.5, height=5, dpi = 300) # save cluster plots
  
  # assign clusters to raw counts for plotting counts
  check_plot$cluster <- as.factor(km$cluster)
  for (jj in seq_along(levels(check_plot$cluster))){
    levels(check_plot$cluster)[jj] <- paste0('Cluster_',
                                         levels(check_plot$cluster)[jj],
                                         ' (n=',
                                         table(check_plot$cluster)[[jj]],
                                         ')')
  }
  check_plot$X <- as.factor(X)
  
  # plot clusters to examine raw count trends in each cluster
  freq_long <- check_plot |>
    pivot_longer(cols = starts_with('Frequency_'),
                 names_to = 'Timepoint',
                 values_to = 'Frequency') |>
    mutate(
      Timepoint = factor(gsub(pattern='Frequency_', replacement='', Timepoint), levels=1:9)
    )
  
  p <- ggplot(freq_long, aes(x = Timepoint, y = Frequency, group = X, color = X)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~ cluster,
               ncol = 3,
               scales = 'free_y') +
    theme(legend.position = 'none') + 
    labs(title = paste0('K-Means Frequency Plots (', df_name, ')'),
         x = 'Timepoint',
         y = 'Normalized Frequency'
    )
  p
  
  plot_save_path <- paste0('results/kmeans/check_plots/', df_name, '_check_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='in', width=4.5, height=5, dpi = 300) # save cluster plots
}

# ----- for testing -----
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
