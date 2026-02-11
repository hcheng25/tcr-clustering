# put all sets into a list for iterating
all_sets <- list(freq = readRDS('rds/freq_neg7.RDS'),
                 freq_z = readRDS('rds/freq_z.RDS'),
                 plus1 = readRDS('rds/plus1.RDS'),
                 plus1_z = readRDS('rds/plus1_z.RDS'),
                 log_foldchange = readRDS('rds/log_foldchange.RDS')
                 )
X <- readRDS('rds/X.RDS')