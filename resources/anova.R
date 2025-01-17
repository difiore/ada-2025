library(tidyverse)
library(mosaic)
library(ggpubr)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)
m <- lm(height ~ weight + age, data = d)

m1 <- lm(height ~ weight, data =d)
m2 <- lm(height ~ age, data =d)

summary(m)
summary(m1)
summary(m2)


plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
plot(fitted(m), residuals(m))
histogram(residuals(m))
ggqqplot(residuals(m))
f <- (summary(m)$r.squared * (nrow(d) - 2 - 1)) / ((1 - summary(m)$r.squared) * 2)
pf(q = f, df1 = nrow(d) - 2 - 1, df2 = 2, lower.tail = FALSE)
car::vif(m)
plot(m)


m <- lm(height ~ weight + age + as.factor(gender), data = d)
summary(m)
f <- (summary(m)$r.squared * (nrow(d) - 3 - 1)) / ((1 - summary(m)$r.squared) * 3)
pf(q = f, df1 = nrow(d) - 3 - 1, df2 = 3, lower.tail = FALSE)

plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
plot(m$model$`as.factor(gender)`, residuals(m))
vif(m)

ci <- predict(m, newdata = data.frame(age = 29, gender = "Male", weight = 132), interval = "confidence", level = 0.95)

pi <- predict(m, newdata = data.frame(age = 29, gender = "Male", weight = 132), interval = "prediction", level = 0.95)

library(jtools)
summ(m)
effect_plot(m, pred = weight, interval = TRUE, int.type = "confidence", int.width = 0.95, plot.points = TRUE, data = d)
effect_plot(m, pred = age, interval = TRUE, int.type = "confidence", int.width = 0.95, plot.points = TRUE, data = d)
plot_summs(m, plot.distributions = TRUE)
