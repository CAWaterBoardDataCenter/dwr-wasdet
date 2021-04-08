# Problem: ggplots's geom_area() does not allow to make stepwise curves (a la geom_step) easily. 
# Intead, it always connects the dots directly. 
# Solution: We will add additional rows to the data frame to match the steps.

library(ggplot2)
library(dplyr)

# some random data
df <- data.frame(x = seq(10), y = sample(10))


# ------------ SHORT VERSION ------------------------------

df_areaStep <- bind_rows(old = df, 
                         new = df %>% mutate(y = lag(y)),
                         .id = "source") %>%
  arrange(x, source)

ggplot(df, aes(x,y)) + 
  geom_ribbon(aes(x = x, ymin = 0, ymax = y), data = df_areaStep)


# ------------ LONG VERSION (more explanation) ------------

# demonstration of the default behavior of geom_area()
(p <- ggplot(df, aes(x, y)) + 
   geom_area(alpha = .1) +  
   geom_step(lty = "dotted") + # the stepwise curve we want to match
   geom_point(col = "tomato"))

# We will have to enter the additional steps manually.
# This can be easily done with the lag function: 
# for each data point (x_n,y_n) we add a point (x_n, y_n-1)
df_extraSteps <- mutate(df, y = lag(y))

# As we can see here, this matches the "missing" corners of the steps
p + geom_point(data = df_extraSteps, col = "dodgerblue")

# We make an additional data frame with the original points and the additional steps. 
# We also have to sort it, so the dots get connecte in the right order 
# (with increasing x, but within each x the new dot is drawn first)
df_areaStep <- bind_rows(old = df, 
                         new = df_extraSteps, 
                         .id = "source") %>% 
  arrange(x, source)

# That's it. We can now use geom_ribbon() to draw the filled step function.
ggplot(df, aes(x, y)) + 
  geom_step(lty = "dotted") +
  geom_point(col = "tomato") +
  geom_point(data = df_extraSteps, col = "dodgerblue") +
  geom_ribbon(data = df_areaStep, aes(ymin = 0, ymax = y), alpha = .1)

