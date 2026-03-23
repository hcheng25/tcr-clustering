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

# ----- for testing -----
agglo_kmeans <- function(df, df_name, y_lab, B=50){
  df <- df |>
    mutate(across(starts_with('Frequency_'), \(x) col_kmeans(x, B=B), .names='{.col}_cluster')) |>
    # combine each tcr's column kmeans clusters into a unique "fingerprint" cluster label
    mutate(agg_cluster = apply(X = select(cur_data(), ends_with('_cluster')),
                               MARGIN = 1,
                               FUN = paste0,
                               collapse = '_')) |>
    select(-(starts_with('Frequency_') & ends_with('_cluster')))
  
  df_meds <- df |>
    group_by(agg_cluster) |>
    summarise(across(starts_with('Frequency_'), \(x) median(x, na.rm=TRUE))) |>
    column_to_rownames('agg_cluster')
  
  df_matrix <- rcorr(t(df_meds)) # want to compute correlation across clusters, which are originally in rows
  
  flattened <- flattenCorrMatrix(df_matrix$r, df_matrix$P) |>
    filter(r>0.8, p<0.05) # select those that are highly correlated with signficant p
  
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
  final_cluster <- as.integer(final_cluster)
  
  df$cluster <- final_cluster
  
  df$cluster <- as.factor(df$cluster)
  for (jj in seq_along(levels(df$cluster))){
    levels(df$cluster)[jj] <- paste0('Cluster_',
                                     levels(df$cluster)[jj],' (n=',
                                     table(df$cluster)[[jj]],
                                     ')')
  }
  
  df$X <- as.factor(X)
  
  # plot clusters to examine z-score trends in each cluster
  freq_long <- df |>
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
  
  plot_save_path <- paste0('results/agglo_kmeans/freq_plots/', df_name, '_cluster_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='in', width=4.5, height=5, dpi = 300) # save cluster plots
  
  # assign clusters to raw counts for plotting counts
  check_plot$cluster <- final_cluster
  check_plot$cluster <- as.factor(check_plot$cluster)
  for (jj in seq_along(levels(check_plot$cluster))){
    levels(check_plot$cluster)[jj] <- paste0('Cluster_',
                                     levels(check_plot$cluster)[jj],' (n=',
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
  
  plot_save_path <- paste0('results/agglo_kmeans/check_plots/', df_name, '_check_plots.png')
  ggsave(filename = plot_save_path, plot = p, units='in', width=4.5, height=5, dpi = 300) # save cluster plots
}

ii <- 1
agglo_kmeans(df = all_sets[[ii]],
                df_name = names(all_sets[ii]),
                y_lab = y_lab[ii],
                B = 5) # adjusted to lower number for testing function

# ----- actual run -----
# for (ii in seq_along(all_sets)){
#   gap_stat_kmeans(df = all_sets[[ii]],
#                   df_name = names(all_sets[ii]),
#                   y_lab = y_lab[ii],
#                   B = 50) 
# }
