packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang', 'dynamicTreeCut')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/hierarchical')){
  dir.create('results/hierarchical')
  dir.create('results/hierarchical/freq_plots')
  dir.create('results/hierarchical/check_plots')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

check_plot <- all_sets$freq # for visually checking clusters

# ----- hierarchical clustering -----
hier_fit <- function(df, df_name, y_lab){
  # use pearson distance
  dist_matrix <- as.dist(1-cor(t(df), method='pearson')) # calculate distance in rows using t()
  
  # linkage method - ward.D2 as default
  hier_clust <- hclust(dist_matrix, method='ward.D2')
  
  # use dynamicTreeCut to prune tree
  clusters <- cutreeDynamic(dendro = hier_clust,
                            distM = as.matrix(dist_matrix),
                            deepSplit = 2, # can tune
                            minClusterSize=5)
  
  # put cluster data into original data frame
  set_with_clusters <- df
  set_with_clusters$cluster <- as.factor(clusters)
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
  
  plot_save_path <- paste0('results/hierarchical/freq_plots/', df_name, '_cluster_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='px', width=3000, height=5000) # save cluster plots
  
  # assign clusters for plotting check_plot
  check_plot$cluster <- as.factor(clusters)
  for (jj in seq_along(levels(check_plot$cluster))){
    levels(check_plot$cluster)[jj] <- paste0('Cluster_',
                                              levels(check_plot$cluster)[jj],
                                              ' (n=',
                                              table(check_plot$cluster)[[jj]],
                                              ')')
  }
  check_plot$X <- as.factor(X)
  
  # plot clusters to examine frequency trends in each cluster
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
    labs(title = paste0('Cluster plots for log10(counts) of ', df_name),
         x = 'Timepoint',
         y = 'Normalized Frequency'
    )
  p
  
  plot_save_path <- paste0('results/hierarchical/check_plots/', df_name, '_check_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='px', width=3000, height=5000) # save cluster plots
}

# set up y labels to sequence through
y_lab <- c('Normalized Frequency',
           'Normalized Frequency (z-score)',
           'Normalized Pseudocount Frequency',
           'Normalized Pseudocount Frequency (z-score)',
           'Log Fold-change from Previous Timepoint')


# ----- test line -----
hier_fit(df = all_sets[[1]],
         df_name = names(all_sets[1]),
         y_lab = y_lab[1])

# ----- actual run -----
for (ii in seq_along(all_sets)){
  hier_fit(df = all_sets[[ii]],
           df_name = names(all_sets[ii]),
           y_lab = y_lab[ii])
}

