# put all sets into a list for iterating
all_sets <- list(freq_z = readRDS('rds/freq_z.RDS'),
                 plus1 = readRDS('rds/plus1.RDS'),
                 log_foldchange = readRDS('rds/log_foldchange.RDS')
                 )
X <- readRDS('rds/X.RDS')
check_plot <- readRDS('rds/freq_neg7.RDS') # for visually checking clusters

# set up y labels to sequence through
y_lab <- c('Frequency (z-score)',
           'Pseudocount Frequency',
           'Log Fold-change from Previous Timepoint')