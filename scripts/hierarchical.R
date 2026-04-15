packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang', 'dynamicTreeCut', 'clusterSim', 'clValid', 'data.table')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/hierarchical')){
  dir.create('results/hierarchical')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# ----- hierarchical clustering -----
hier_fit <- function(df){
  # use pearson distance
  dist_matrix <- as.dist(1-cor(t(df), method='pearson')) # calculate distance in rows using t()
  
  # linkage method - ward.D2 as default
  hier_clust <- hclust(dist_matrix, method='ward.D2')
  
  # use dynamicTreeCut to prune tree
  final_cluster <- cutreeDynamic(dendro = hier_clust,
                            distM = as.matrix(dist_matrix),
                            deepSplit = 2, # can tune
                            minClusterSize=5)
  
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
    labs(title = paste0('Hierarchical Frequency Plots (', df_name, ')'),
         x = 'Timepoint',
         y = 'Normalized Frequency'
    )
  
  return(p)
}

# ----- test run -----
# ii <- 1
# test_df <- hier_fit(df = all_sets[[ii]]) # adjusted to lower number for testing function
# 
# test_df_w_other <- group_other(df = test_df)
# 
# test_plot <- cluster_plot(df = test_df,
#                           check_df = check_plot,
#                           X = X,
#                           y_lab = y_lab[ii],
#                           df_name = names(all_sets[ii]),
#                           ncol = 8,
#                           linewidth=1.5)
# 
# test_plot_w_other <- cluster_plot(df = test_df_w_other,
#                                   check_df=check_plot,
#                                   X = X,
#                                   y_lab = y_lab[ii],
#                                   df_name = names(all_sets[ii]),
#                                   ncol = 8,
#                                   linewidth=1)
# test_plot
# test_plot_w_other
# 
# # silhouette - higher is better, >0.5 indicates pretty good clustering
# # correlation distance matrix calculation
# feature_matrix <- test_df |>
#   dplyr::select(starts_with('Frequency_')) |>
#   as.matrix()
# dist_mat <- as.dist(1 - cor(t(feature_matrix)))
# 
# sil <- silhouette(as.integer(test_df$final_cluster), dist_mat)
# mean(sil[,3])
# 
# # DBI - lower is better
# dbi <- index.DB(feature_matrix, as.integer(test_df$final_cluster), d = "correlation")
# 
# # Dunn index - higher is better
# dunn <- dunn(distance = dist_mat, clusters = as.integer(test_df$final_cluster))
# 
# metrics <- data.frame(Metric = c('Silhouette', 'DBI', 'Dunn'),
#                       Value = c(mean(sil[,3]), dbi$DB, dunn))
# fwrite(metrics, file = paste0('results/hierarchical/', names(all_sets[ii]), '_metrics.txt'))

# ----- actual run -----
for (ii in seq_along(all_sets)){
  df <- hier_fit(df = all_sets[[ii]])
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
  
  cluster_path <- paste0('results/hierarchical/', names(all_sets[ii]), '_cluster_plots.png')
  other_path <- paste0('results/hierarchical/', names(all_sets[ii]), '_cluster_plots_w_other.png')
  
  ggsave(filename = cluster_path, plot = plot, units='in', width=30, height=15, dpi = 300)
  ggsave(filename = other_path, plot = other_plot, units='in', width=20, height=10, dpi = 300)
  
  # silhouette - higher is better, >0.5 indicates pretty good clustering
  # correlation distance matrix calculation
  feature_matrix <- df |>
    dplyr::select(starts_with('Frequency_')) |>
    as.matrix()
  dist_mat <- as.dist(1 - cor(t(feature_matrix)))
  
  sil <- silhouette(as.integer(df$final_cluster), dist_mat)
  mean(sil[,3])
  
  # DBI - lower is better
  dbi <- index.DB(feature_matrix, as.integer(df$final_cluster), d = "correlation")
  
  # Dunn index - higher is better
  dunn <- dunn(distance = dist_mat, clusters = as.integer(df$final_cluster))
  
  metrics <- data.frame(Metric = c('Silhouette', 'DBI', 'Dunn'),
                        Value = c(mean(sil[,3]), dbi$DB, dunn))
  fwrite(metrics, file = paste0('results/hierarchical/', names(all_sets[ii]), '_metrics.txt'))
}