# install necessary packages
packages <- c('tidyverse', 'cluster', 'ggplot2', 'rlang', 'dynamicTreeCut', 'Hmisc')
lapply(packages, install.packages)

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

if(!dir.exists('results')){
  dir.create('results')
}

if(!dir.exists('rds')){
  dir.create('rds')
}