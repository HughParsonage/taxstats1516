library(data.table)
library(synthpop)
library(hutils)

sample_file_1516 <-
  fread("~/ozTaxData/data-raw/2016_sample_file.csv", logical01 = FALSE)

sample_file_1516_synth <-
  syn(as.data.frame(sample_file_1516), method = "ctree")



