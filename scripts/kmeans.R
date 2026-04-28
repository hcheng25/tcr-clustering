packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang', 'clusterSim', 'clValid', 'data.table')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/kmeans')){
  dir.create('results/kmeans')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# ----- using gap statistic to select number of clusters -----
# counts for plotting the non normalized plots based on cluster assignments
gap_stat_kmeans <- function(df, B=50){
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
  final_cluster <- km$cluster

  # save clusters in integer form
  df$cluster <- as.integer(final_cluster)
  
  # final cluster as a factor
  df$final_cluster <- as.factor(final_cluster)
  for (jj in seq_along(levels(df$final_cluster))){
    levels(df$final_cluster)[jj] <- paste0('Cluster_',
                                           levels(df$final_cluster)[jj],' (n=',
                                           table(df$final_cluster)[[jj]],
                                           ')')
  }
  
  return(df)
}

# put "clusters" of n=1 into an "other" category
group_other <- function(df){
  final_cluster <- df$cluster
  
  cluster_criteria <- which(table(final_cluster)==1)
  cluster_counts_other <- names(cluster_criteria)
  other_total <- sum(table(final_cluster)[cluster_criteria])
  
  # label "other" as cluster0 temporarily
  for (ii in seq_along(final_cluster)){
    if(final_cluster[ii] %in% cluster_counts_other){ final_cluster[ii] <- 0 }
  }
  
  final_cluster <- as.factor(final_cluster)
  for (jj in seq_along(levels(final_cluster))){
    levels(final_cluster)[jj] <- paste0('Cluster_',
                                        levels(final_cluster)[jj],' (n=',
                                        table(final_cluster)[[jj]],
                                        ')')
  }
  
  # rename cluster 0 to "other"
  levels(final_cluster) <- gsub(pattern='^Cluster_0', replacement='Other', x=levels(final_cluster))
  
  df$final_cluster <- final_cluster
  
  return(df)
}
  
# plot frequency to check clustering
cluster_plot <- function(df, check_df, X, y_lab, df_name, ncol = 8, linewidth=1.5){
  # assign clusters to raw counts for plotting counts
  check_df$cluster <- df$final_cluster
  check_df$X <- as.factor(X)
  
  # plot clusters to examine raw count trends in each cluster
  freq_long <- check_df |>
    pivot_longer(cols = starts_with('Frequency_'),
                 names_to = 'Timepoint',
                 values_to = 'Frequency') |>
    mutate(
      Timepoint = factor(gsub(pattern='Frequency_', replacement='', Timepoint), levels=1:9)
    )
  
  p <- ggplot(freq_long, aes(x = Timepoint, y = Frequency, group = X, color = X)) +
    geom_line(alpha = 0.3, linewidth=linewidth) +
    facet_wrap(~ cluster,
               ncol = ncol,
               scales = 'free_y') +
    theme(legend.position = 'none') + 
    labs(title = paste0('K-Means Frequency Plots (', df_name, ')'),
         x = 'Timepoint',
         y = 'Normalized Frequency'
    )
  
  return(p)
}

perf_metrics <- function(matrix, final_cluster){
  # silhouette - higher is better, >0.5 indicates pretty good clustering
  # correlation distance matrix calculation
  dist_mat <- as.dist(1 - cor(t(matrix)))
  
  sil <- silhouette(as.integer(final_cluster), dist_mat)
  sil <- mean(sil[,3])
  
  # DBI - lower is better
  dbi <- index.DB(matrix, as.integer(final_cluster), d = "correlation")
  dbi <- dbi$DB
  
  # Dunn index - higher is better
  dunn <- dunn(distance = dist_mat, clusters = as.integer(final_cluster))
  
  return(c(sil, dbi, dunn))
}

# ----- actual run -----
for (ii in seq_along(all_sets)){
  df <- gap_stat_kmeans(df = all_sets[[ii]],
                        B = 50)
  df_w_other <- group_other(df = df)
  
  plot <- cluster_plot(df = df,
                       check_df = check_plot,
                       X = X,
                       y_lab = y_lab[ii],
                       df_name = names(all_sets[ii]),
                       ncol = 8,
                       linewidth=1.5)
  
  other_plot <- cluster_plot(df = df_w_other,
                               check_df=check_plot,
                               X = X,
                               y_lab = y_lab[ii],
                               df_name = names(all_sets[ii]),
                               ncol = 8,
                               linewidth=1)
  
  cluster_path <- paste0('results/kmeans/', names(all_sets[ii]), '_cluster_plots.png')
  other_path <- paste0('results/kmeans/', names(all_sets[ii]), '_cluster_plots_w_other.png')
  
  ggsave(filename = cluster_path, plot = plot, units='in', width=30, height=15, dpi = 300)
  ggsave(filename = other_path, plot = other_plot, units='in', width=20, height=10, dpi = 300)
  
  cluster_matrix <- df |>
    dplyr::select(starts_with('Frequency_')) |>
    as.matrix()
  
  check_matrix <- check_plot |>
    dplyr::select(starts_with('Frequency_')) |>
    as.matrix()

  metrics <- data.frame(Metric = c('Silhouette', 'DBI', 'Dunn'),
                        Norm_Values = perf_metrics(matrix=cluster_matrix, final_cluster=df$final_cluster),
                        Original_Values = perf_metrics(matrix=check_matrix, final_cluster=df$final_cluster))
  fwrite(metrics, file = paste0('results/kmeans/', names(all_sets[ii]), '_metrics.txt'))
}



