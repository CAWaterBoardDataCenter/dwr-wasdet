library(pdftables)
library(tidyr)
library(dplyr)

fname <- "SJWSI-20220201"

old_wd <- getwd()
setwd("/Users/jyeazell/R/GitHub/work/dwr-wasdet/supply-data/sjwsi")

convert_pdf(paste0(fname,".pdf"), paste0(fname, ".csv"))
