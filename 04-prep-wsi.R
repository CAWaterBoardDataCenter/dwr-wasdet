
library(readr)
library(pdftables)

i_file <- "./supply-data/srwsi/SRWSI-20210201.pdf" 
o_file <- "./supply-data/srwsi/SRWSI-20210201.csv"

# convert_pdf(i_file, o_file, format = "csv", api_key = Sys.getenv("pdftable_api_key"))
# get_remaining(api_key = Sys.getenv("pdftable_api_key"))


wsi <- read_csv("./supply-data/wsi/b120-wsi-20210201.csv")
