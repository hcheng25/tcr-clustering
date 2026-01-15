# load all sets of normalized data
freq <- readRDS('rds/freq_neg7.RDS')
freq_plus_one_method <- readRDS('rds/freq_plus_one_method.RDS')

# put all sets into a list for iterating
all_sets <- list(freq = freq,
                 freq_plus_one_method = freq_plus_one_method
                 )