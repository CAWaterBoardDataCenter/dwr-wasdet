library(dplyr)
library(readr)
library(janitor)
library(DataExplorer)
library(ggplot2)

supply <- read_csv("./supply-data/SupplyData_All.csv")

supply <- supply %>% 
  rename(huc8_name = huc8)


create_report(supply)

test_supply <- filter(supply, huc8_name == "McCloud")

ggplot(data = test_supply, aes(x = factor(cy_mo), y = cfs)) +
  geom_jitter(aes(color = factor(cy)),
              alpha = 0.5,
              width = 0.2) +
  
  geom_boxplot(alpha = 0.3,
               outlier.alpha = 0) + 
#  facet_wrap(facets = vars(source_gage)) +
  scale_y_continuous(limits = c(0, NA),
                     breaks = seq(0, max(test_supply$cfs), by = 1000),
                     minor_breaks = seq(0, max(test_supply$cfs), by = 250)) +
  theme_bw() +
  labs(title = "Historical Flow at MSS", 
       x = "Month", 
       y = "Flow (cfs)", 
       color = "Year\n")
