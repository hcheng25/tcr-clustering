---
title: 'TCR Clustering Results'
date: '12FEB2026'
---
# Normalization Methods

+-----------------+-----------------------------------------------------------------------------------------+
| Name            | Method                                                                                  |
+=================+=========================================================================================+
| Frequency       | -Original frequency values from the dataset                                             |
|                 |                                                                                         |
|                 | -Zeroes (which would be undefined after log10) changed to -7 after log everything else  |
+-----------------+-----------------------------------------------------------------------------------------+
| Freq (z-score)  | -Z-score of Freq across TCRs                                                            |
+-----------------+-----------------------------------------------------------------------------------------+
| Plus1           | -Add psuedocount of 1 to all raw counts (prevent log(0))                                |
|                 |                                                                                         |
|                 | -Divide by TCR row total and log10                                                      |
+-----------------+-----------------------------------------------------------------------------------------+
| Log Foldchange  | -Add psuedocount of 1 to all raw counts (prevent dividing by 0)                         |
|                 |                                                                                         |
|                 | -Starting at second timepoint, divide by previous timepoint, then take log10            |
+-----------------+-----------------------------------------------------------------------------------------+

# K-Means

# Freq (z-score)

:::columns

:::column
![Normalized plot](results/kmeans/freq_plots/freq_z_cluster_plots.png){ width=100% }
:::

:::column
![Projected to frequency](results/kmeans/check_plots/freq_z_check_plots.png){ width=100% }
:::

:::

# Plus1

:::columns

:::column
![Normalized plot](results/kmeans/freq_plots/plus1_cluster_plots.png){ width=100% }
:::

:::column
![Projected to frequency](results/kmeans/check_plots/plus1_check_plots.png){ width=100% }
:::

:::

# Log Fold-Change

:::columns

:::column
![Normalized plot](results/kmeans/freq_plots/log_foldchange_cluster_plots.png){ width=100% }
:::

:::column
![Projected to frequency](results/kmeans/check_plots/log_foldchange_check_plots.png){ width=100% }
:::

:::

-----

# Hierarchical

# Freq (z-score)

:::columns

:::column
![Normalized plot](results/hierarchical/freq_plots/freq_z_cluster_plots.png){ width=100% }
:::

:::column
![Projected to frequency](results/hierarchical/check_plots/freq_z_check_plots.png){ width=100% }
:::

:::

# Plus1

:::columns

:::column
![Normalized plot](results/hierarchical/freq_plots/plus1_cluster_plots.png){ width=100% }
:::

:::column
![Projected to frequency](results/hierarchical/check_plots/plus1_check_plots.png){ width=100% }
:::

:::

# Log Fold-Change

:::columns

:::column
![Normalized plot](results/hierarchical/freq_plots/log_foldchange_cluster_plots.png){ width=100% }
:::

:::column
![Projected to frequency](results/hierarchical/check_plots/log_foldchange_check_plots.png){ width=100% }
:::

:::