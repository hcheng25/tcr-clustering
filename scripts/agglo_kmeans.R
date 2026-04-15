packages <- c('cluster', 'ggplot2', 'rlang', 'Hmisc', 'clusterSim', 'clValid', 'data.table', 'tidyverse')
lapply(packages, library, character.only=TRUE)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results/agglo_kmeans')){
  dir.create('results/agglo_kmeans')
}

set.seed(42)

# load all normalized data sets
source('functions/load_norms.R')

# ----- using gap statistic to cluster TCR clones in each timepoint -----
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

# ----- agglomerative kmeans clustering -----
agglo_kmeans <- function(df, B=50, r_thresh=0.8, p_thresh=0.05){
  df <- df |>
    mutate(across(starts_with('Frequency_'), \(x) col_kmeans(x, B=B), .names='{.col}_cluster')) |>
    # combine each tcr's column kmeans clusters into a unique "fingerprint" cluster label
    mutate(agg_cluster = apply(X = dplyr::select(cur_data(), ends_with('_cluster')),
                               MARGIN = 1,
                               FUN = paste0,
                               collapse = '_')) |>
    dplyr::select(-matches('^Frequency_.*_cluster$'))
  
  df_meds <- df |>
    group_by(agg_cluster) |>
    summarise(across(starts_with('Frequency_'), \(x) median(x, na.rm=TRUE))) |>
    column_to_rownames('agg_cluster')
  
  df_matrix <- rcorr(t(df_meds)) # want to compute correlation across clusters, which are originally in rows
  
  flattened <- flattenCorrMatrix(df_matrix$r, df_matrix$P) |>
    filter(r>r_thresh, p<p_thresh) # select those that are highly correlated with signficant p

  # use correlated clusters to make list of clusters that should be combined
  combos <- list() # initiate list
  for (ii in seq(nrow(flattened))){
    sorted <- unique(unlist(combos))
    label1 <- flattened$row[ii]
    label2 <- flattened$column[ii]

    if (label1 %in% sorted & label2 %in% sorted){
      # if both are already sorted, combine the groups
      for (jj in seq_along(combos)){
        if(label1 %in% combos[[jj]]){
          label1_index <- jj
          label1_group <- combos[[jj]]
        }
        if(label2 %in% combos[[jj]]){
          label2_index <- jj
          label2_group <- combos[[jj]]
        }
      }
      # make new combined group and exclude original separate groups
      if(label1_index != label2_index){
        combos[[length(combos)+1]] <- c(label1_group, label2_group)
        combos <- combos[c(-label1_index, -label2_index)]
      }

    }else if (label1 %in% sorted){
      # if label1 already in the list find the group that label1 is in and add label2 to it
      for (jj in seq_along(combos)){
        if(label1 %in% combos[[jj]]){
          combos[[jj]] <- c(combos[[jj]], label2)
          break
        }
      }

    }else if(label2 %in% sorted){
      # if label2 already in the list find the group that label2 is in and add label1 to it
      for (jj in seq_along(combos)){
        if(label2 %in% combos[[jj]]){
          combos[[jj]] <- c(combos[[jj]], label1)
          break
        }
      }

    }else{
      # if neither is present, add both to new element of list
      combos[[length(combos)+1]] <- c(label1, label2)
    }
  }

  # include the labels that were not combined with other labels
  labels <- unique(df$agg_cluster)
  for (ii in seq_along(labels)){
    if (!(labels[ii] %in% unique(unlist(combos)))){
      combos[[length(combos)+1]] <- labels[ii]
    }
  }

  names(combos) <- seq_along(combos)

  # vector of cluster labels, renamed
  cluster_labels <- unique(unlist(combos))
  new_labels <- c()
  for (ii in cluster_labels){
    for (jj in seq_along(combos)){
      if (ii %in% combos[[jj]]){
        new_labels <- c(new_labels, jj)
        break
      }
    }
  }

  final_cluster <- df$agg_cluster
  for (ii in seq_along(final_cluster)){
    if(final_cluster[ii] %in% cluster_labels){
      final_cluster[ii] <- new_labels[cluster_labels==final_cluster[ii]]
    }
  }

  # save clusters in integer form
  df$cluster <- as.integer(final_cluster)
  
  # final cluster is a factor
  df$final_cluster <- as.factor(final_cluster)

  df$final_cluster <- as.factor(df$final_cluster)
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
    labs(title = paste0('Agglomerative K-Means Frequency Plots (', df_name, ')'),
         x = 'Timepoint',
         y = 'Normalized Frequency'
    )

  return(p)
}

# ----- test run -----
# ii <- 1
# test_df <- agglo_kmeans(df = all_sets[[ii]],
#                         B = 5, # adjusted to lower number for testing function
#                         r_thresh=0.9,
#                         p_thresh=0.05)
# test_df_w_other <- group_other(df = test_df)
# 
# test_plot <- cluster_plot(agglo_df = test_df,
#                           check_df=check_plot,
#                           X = X,
#                           y_lab = y_lab[ii],
#                           df_name = names(all_sets[ii]))
# test_plot_w_other <- cluster_plot(agglo_df = test_df_w_other,
#                                   check_df=check_plot,
#                                   X = X,
#                                   y_lab = y_lab[ii],
#                                   df_name = names(all_sets[ii]))
# test_plot
# test_plot_w_other
# 
# # silhouette - higher is better, >0.5 indicates pretty good clustering
# # correlation distance matrix calculation
# feature_matrix <- df |>
#   select(starts_with('Frequency_')) |>
#   as.matrix()
# dist_mat <- as.dist(1 - cor(t(feature_matrix)))
# 
# sil <- silhouette(as.integer(df$final_cluster), dist_mat)
# mean(sil[,3])
# 
# # DBI - lower is better
# dbi <- index.DB(feature_matrix, as.integer(df$final_cluster), d = "correlation")
# 
# # Dunn index - higher is better
# dunn <- dunn(distance = dist_mat, clusters = as.integer(df$final_cluster))
# 
# metrics <- data.frame(Metric = c('Silhouette', 'DBI', 'Dunn'),
#                       Value = c(mean(sil[,3]), dbi$DB, dunn))
# fwrite(metrics, file = paste0('results/agglo_kmeans/', names(all_sets[ii]), '_metrics.txt'))
# 
# # results from these clustering metrics
# # silhouette and dbi indicate pretty good clustering, but dunn index indicates poor clustering, likely bc
# # dunn index is highly sensitive to n=1 groups and that pushes its measure of clustering down due to high numbers
# # of n=1 groups

# ----- actual run -----
for (ii in seq_along(all_sets)){
  if (ii == 3){ next } # method does not work with log fold change

  df <- agglo_kmeans(df = all_sets[[ii]],
                     B = 100,
                     r_thresh=0.9,
                     p_thresh=0.05)

  df_w_other <- group_other(df = df)

  plot <- cluster_plot(df = df,
                       check_df=check_plot,
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

  cluster_path <- paste0('results/agglo_kmeans/', names(all_sets[ii]), '_cluster_plots.png')
  other_path <- paste0('results/agglo_kmeans/', names(all_sets[ii]), '_cluster_plots_w_other.png')

  ggsave(filename = cluster_path, plot = plot, units='in', width=30, height=15, dpi = 300)
  ggsave(filename = other_path, plot = other_plot, units='in', width=20, height=10, dpi = 300)
  
  # silhouette - higher is better, >0.5 indicates pretty good clustering
  # correlation distance matrix calculation
  feature_matrix <- df |>
    select(starts_with('Frequency_')) |>
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
  fwrite(metrics, file = paste0('results/agglo_kmeans/', names(all_sets[ii]), '_metrics.txt'))
}


