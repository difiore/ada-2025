# multiple Regression/ANCOVA
library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z <- read_csv(f, col_names = TRUE)
m <- lm(height ~ weight + age, data = z)

summary(m)
plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
plot(fitted(m), residuals(m))
summary(aov(m))

histogram(residuals(m))
ggqqplot(residuals(m))

f <- (summary(m)$r.squared * (nrow(d) - 2 - 1)) / ((1 - summary(m)$r.squared) * 2)
pf(q = f, df1 = nrow(d) - 2 - 1, df2 = 2, lower.tail = FALSE)

car::vif(m)
plot(m)

(p <- pf(f, df1 = 2, df2 = 997, lower.tail = FALSE))
(p <- pf(f, df1 = summary(m)$fstatistic[2],
         df2 = summary(m)$fstatistic[3],
         lower.tail = FALSE))

m <- lm(height ~ weight + age + gender , data = z)
summary(m)

f <- (summary(m)$r.squared * (nrow(d) - 3 - 1)) / ((1 - summary(m)$r.squared) * 3)
pf(q = f, df1 = nrow(d) - 3 - 1, df2 = 3, lower.tail = FALSE)

plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
boxplot(residuals(m) ~ m$model$gender)
plot(fitted(m), residuals(m))
library(car)
vif(m)

temp <- lm(weight ~ age + gender, data = z)
s <- summary(temp)
1/(1-s$r.squared)

# confidence and prediction intervals
(ci <- predict(m,
               newdata = data.frame(age = 29, gender = "Male", weight = 132),
               interval = "confidence",
               level = 0.95))

(pi <- predict(m,
               newdata = data.frame(age = 29, gender = "Male", weight = 132),
               interval = "prediction",
               level = 0.95))

(p <- ggplot(data = z, aes(x = age, y = height)) +
    geom_point() +
    geom_smooth(method = lm, ))

m <- lm(height ~ weight, data = z)

# effects plots
library(jtools)
summ(m)
effect_plot(m,
            pred = weight,
            interval = TRUE,
            int.type = "confidence",
            int.width = 0.95,
            plot.points = TRUE)

m1 <- lm(height ~ weight + age + gender, data = z)

effect_plot(m1,
            pred = weight,
            interval = TRUE,
            int.type = "confidence",
            int.width = 0.95,
            plot.points = TRUE)

effect_plot(m1, pred = weight,
            interval = TRUE,
            int.type = "prediction",
            int.width = 0.95,
            plot.points = TRUE)

effect_plot(m1, pred = age,
            interval = TRUE,
            int.type = "prediction",
            int.width = 0.95,
            plot.points = TRUE)

effect_plot(m1, pred = gender,
            interval = TRUE,
            int.type = "prediction",
            int.width = 0.95,
            plot.points = TRUE)

plot_summs(m1)
plot_summs(m1,
           plot.distributions = TRUE,
           rescale.distributions = TRUE)

plot_summs(m, m1,
           plot.distributions = TRUE,
           rescale.distributions = TRUE)
