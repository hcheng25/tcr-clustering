packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang')
lapply(packages, install.packages)
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# use elbow method
# initialize best_k_elbow to record best k as recorded by second derivative method below
best_k_elbow <- integer(length(all_sets))

# iterate from k=1 to k=10 to find WSS and plot elbow plots
for (ii in seq_along(all_sets)){
  wss <- 1:10 # initialize within-cluster sum of squares
  for (k in 1:10){
    km <- kmeans(all_sets[[ii]][-1], centers = k, nstart = 25)
    wss[k] <- km$tot.withinss
  }
  
  wss_plot <- data.frame(x = 1:10,
                         y = wss)
  
  # use second derivative to detect which point corresponds to elbow
  first_diff <- diff(wss)
  second_diff <- diff(first_diff)
  # first local minimum of second diff i.e. where the rate of decrease first reaches a local minimum
  # compare each value of the second differential to the previous value
  elbow_pt <- which(second_diff[-1] > second_diff[-length(second_diff)])[1] + 1
  best_k_elbow[ii] <- elbow_pt
  
  p <- ggplot(wss_plot, aes(x=x, y=y)) +
    geom_line() +
    geom_point() +
    annotate('point', x = elbow_pt, y = wss[elbow_pt], color = 'green', size = 4) +
    scale_x_continuous(breaks=1:10) + 
    labs(title = paste0('Elbow plot for ', names(all_sets)[ii]),
         x = '# of clusters K',
         y = 'WSS'
    )
    
  plot_save_path <- paste0('results/', names(all_sets)[ii], '_elbow.png')
  ggsave(filename = plot_save_path, plot = p, width = 11, height = 5, dpi = 300) # save elbow plots
}

names(best_k_elbow) <- names(all_sets)

# based on visual examination of elbow plot, 3 clusters is better as elbow for freq+1 method data set
best_k_elbow[2] <- 3

for (ii in seq_along(all_sets)){
  km <- kmeans(all_sets[[ii]][-1], centers = best_k_elbow[[ii]], nstart = 25)
  set_with_clusters <- all_sets[[ii]]
  cluster_label <- km$cluster
  cluster_with_n <- vector(mode='character', length=length(cluster_label))
  for (jj in seq_along(cluster_label)){
    cluster_with_n[jj] <- paste0('Cluster_', cluster_label[jj], ' (n=', table(cluster_label)[[cluster_label[jj]]], ')')
  }
  set_with_clusters$cluster <- cluster_with_n
  
  # plot clusters to examine trends in each cluster
  freq_long <- set_with_clusters |>
    pivot_longer(cols = starts_with('Frequency_'),
                 names_to = 'Timepoint',
                 values_to = 'Frequency') |>
    mutate(
      Timepoint = factor(gsub(pattern='Frequency_', replacement='', Timepoint), levels=1:9),
      X = as.factor(X)
    )
  
  p <- ggplot(freq_long, aes(x = Timepoint, y = Frequency, group = X, color = X)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~ cluster) +
    theme(legend.position = 'none') + 
    labs(title = paste0('Cluster plots for ', names(all_sets)[ii]),
         x = 'Timepoint',
         y = 'Normalized Frequency'
    )
  
  plot_save_path <- paste0('results/', names(all_sets)[ii], '_cluster_plots.png')
  ggsave(filename = plot_save_path, plot = p, width = 11, height = 5, dpi = 300) # save cluster plots
}

test_frame <- all_sets[[1]]

# use gap statistic to select number of clusters
gap_stat <- clusGap(x = test_frame,
                    FUN = kmeans,
                    K.max = 10,
                    nstart = 25
                    )
print(gap_stat, method='Tibs2001SEmax')
plot(gap_stat)
# note: trouble with convergence

km <- kmeans(test_frame, centers = 5, nstart = 25)
set_with_clusters <- test_frame
set_with_clusters$cluster <- paste0('Cluster_', km$cluster)
X <- all_sets[[1]][[1]]
set_with_clusters$X <- X

# plot clusters to examine trends in each cluster
freq_long <- set_with_clusters |>
  pivot_longer(cols = starts_with('Frequency_'),
               names_to = 'Timepoint',
               values_to = 'Frequency') |>
  mutate(
    Timepoint = factor(gsub(pattern='Frequency_', replacement='', Timepoint), levels=1:9),
    X = as.factor(X)
  )

p <- ggplot(freq_long, aes(x = Timepoint, y = Frequency, group = X, color = X)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ cluster) +
  theme(legend.position = 'none') + 
  labs(title = paste0('Cluster plots for ', names(all_sets)[ii]),
       x = 'Timepoint',
       y = 'Normalized Frequency'
  )
p
