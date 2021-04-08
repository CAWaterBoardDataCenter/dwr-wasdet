library(ggplot2)
library(tidyr)
library(lubridate)

set.seed(5150)


demand1 <- tibble(plot_date = seq(from = as.Date("2021-01-01"),
                                 by = "month",
                                 length.out = 12),
                 cfs = runif(12) * 1000,
                 priority = "A",
                 plot_group = "demand")

demand2 <- tibble(plot_date = seq(from = as.Date("2021-01-01"),
                                  by = "month",
                                  length.out = 12),
                  cfs = runif(12) * 250,
                  priority = "B",
                  plot_group = "demand")

supply <- tibble(plot_date = seq(from = as.Date("2021-01-01"),
                                 by = "month",
                                 length.out = 12),
                 cfs = runif(12) * 750,
                 priority = "B",
                 plot_group = "supply")

plot_data <- bind_rows(demand1, demand2)

# g1 <- ggplot(data = plot_data,
#               mapping = aes(x = plot_date,
#                             y = cfs))
# 
# # geom_area
# g1 + geom_area(position = "stack",
#                mapping = aes(fill = priority))


## edge demand1 --

old <-  plot_data

new <- plot_data %>% 
  mutate(plot_date = ceiling_date(x = plot_date,
                                  unit = "month") - 1)

new_plot_data <- bind_rows(old = old,
                               new = new,
                               .id = "source") %>% 
  arrange(plot_date, priority)





g <- ggplot(data = new_plot_data,
            mapping = aes(x = plot_date,
                          y = cfs))

# Demand
g + geom_area(mapping = aes(fill = priority),
              position = "stack")
