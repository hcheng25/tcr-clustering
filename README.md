# Exploratory TCR Clustering

## Repository structure

### Folders
- `data`
    - `.csv` for original data with normalized frequencies, calculated by taking log10 of TCR counts, replacing undefined outputs of 0 with -7 (equivalent to a psuedocount of 10^-7)
- `functions`
    - R files containing common load functions used in all clustering scripts
- `scripts`
    - R scripts for setting up, normalizing frequency data, and running clustering methods using each of the normalized sets of data
- `rds`
    - `.RDS` files containing normalized datasets and the row index `X.RDS` from the original frequency data
- `results`
    - plot outputs using original frequencies for each clustering method using each normalization method

## Scripts

### `setup.R`

Installs necessary R packages to run other scripts in repository and creates subdirectories if they are not already present

### `normalizations.R`

Preprocessing of frequency data into several datasets using different normalization methods. Also saves the TCR index of the original frequency data for use in plotting

### `kmeans.R, hierarchical.R, agglo_kmeans.R`

Scripts that run unsupervised clustering according to various methods and plots them to the `results` folder. If applicable, scripts will additionally plot clusters with all singleton TCR clones grouped into one subplot to make the other cluster subplots more readable

## Normalization Methods

### Z-Score

Z-score of log10(frequency) across TCR clones i.e. rowwise

### Plus 1 Pseudocount

Add pseudocount of 1 to all raw counts to prevent log10(0), then divide each value by the TCR row total and take the log10 value

### Log Fold-change

Add pseudocount of 1 to all raw counts to prevent division by 0, and starting at the second timepoint, divide count by the previous timepoint and take log10

## Clustering Methods

### K-Means

Simple clustering using k-means, with the optimal number of clusters k determined using the gap statistic

### Hierarchical

Calculate the Pearson distance between rows, then apply hierarchical clustering using the ward.D2 method. Prune the trea using dynamicTreeCut with a minimum cluster size of 5

### Agglomerative K-Means

First cluster each timepoint for all TCR clones. The resulting cluster combination becomes a composite cluster ID for the clone i.e. \#\_\#\_\#\_\#\_...

For each composite cluster, find the median at each time point, then cluster composite clusters based on correlation between those medians and the p-value (thresholds used: r>0.9, p<0.5). The clusters after combining highly correlated composite clusters become the final clusters.

## Clustering Metrics

### Silhouette Score

### Davies-Bouldin Index (DBI)

### Dunn Index