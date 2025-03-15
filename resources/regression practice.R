# Regression
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)

h <- d$height - mean(d$height)
w <- d$weight - mean(d$weight)
cov <- sum(h*w)/(length(d$height) - 1)
b1 <- cov/var(d$weight)

ssxy <- sum((d$weight - mean(d$weight)) * (d$height - mean(d$height)))

ssx <- sum((d$weight- mean(d$weight))^2)

(b1 <- ssxy/ssx)
(b0 <- mean(d$height) - b1 * mean(d$weight))

m <- lm(height~weight, data = d)
summary(m)

# Centering and Interactive Regression Line
d <- mutate(d, centered_height = height - mean(height))
d <- mutate(d, centered_weight = weight - mean(weight))

slope.test <- function(beta1, data) {
  g <- ggplot(data = data,
              aes(x = centered_weight,
                  y = centered_height))
  g <- g + geom_point()
  g <- g + geom_abline(intercept = 0,
                       slope = beta1,
                       size = 1,
                       color = "blue",
                       alpha = 1/2)
  ols <- sum((data$centered_height - beta1 * data$centered_weight)^2)
  g <- g +
    ggtitle(paste("Slope = ", beta1,
                  "\nSum of Squared Deviations = ",
                  round(ols, 3)))
  g
}

library(manipulate)
manipulate(slope.test(beta1, data = d),
           beta1 = slider(-1, 1, initial = 0, step = 0.005))

cov/(sd(d$height)* sd(d$weight))
cor(d$height, d$weight)

(b1 <- (cor(d$weight, d$height) * sd(d$height))/(sd(d$weight)))

# More Regression
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Street_et_al_2017.csv"
d <- read_csv(f, col_names = TRUE)
m <- lm(formula = ECV ~ Group_size, data = d)
summary(m)


# Regression - ANOVA tables
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)

# Regression model with our zombie apocalypse survivor dataset
m <- lm(data = d, height ~ weight)

# SSY = height - mean(height)
SSY <- sum((m$model$height - mean(m$model$height)) ^ 2)

# SSR = predicted height - mean height
SSR <- sum((m$fitted.values - mean(m$model$height)) ^ 2)

# SSE = height - predicted height
SSE <- sum((m$model$height - m$fitted.values) ^ 2)

# mean overall variance
MSY <- SSY/(nrow(d) - 1)

# mean variance explained by the regression equation
MSR <- SSR/(1)

# mean remaining variance
MSE <- SSE/(nrow(d) - 1 - 1)

# Fratio
fratio <- MSR/MSE

# P value - proportion of F distribution between 0 and fratio
pf(q = fratio, df1 = 1, df2 = 998, lower.tail = FALSE)
1 - pf(q = fratio, df1 = 1, df2 = 998)

summary(m)
rsq <- SSR/SSY

library(mosaic)
plotDist("f", df1 = 1, df2 = 998)
plotDist("f", df1 = 30, df2 = 30)

new_d <- tibble(x = rnorm(1000, mean = 100, sd = 25),
                y = rnorm(1000, mean = 10, sd = 2))
plot(new_d$x, new_d$y)
m <- lm(y ~ x, data = new_d)

# SSY
SSY <- sum((m$model$y - mean(m$model$y)) ^ 2)

# SSR
SSR <- sum((m$fitted.values - mean(m$model$y)) ^ 2)

# SSE
SSE <- sum((m$model$y - m$fitted.values) ^ 2)

# mean overall variance
MSY <- SSY/(nrow(new_d) - 1)

# mean variance explained by the regression equation
MSR <- SSR/(1)

# mean remaining variance
MSE <- SSE/(nrow(new_d) - 1 - 1)

# Fratio
fratio <- MSR/MSE

anova(m)
pf(q = fratio, df1 = 1, df2 = 998, lower.tail = FALSE)
plotDist("f", df1 = 1, df2 = 998)


library(tidyverse)
# Simple linear regression
library(mosaic)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)
m <- lm(data = d, height ~ weight)
(tidy_m <- broom::tidy(m))
# get just slope coefficient
(obs_slope <- tidy_m |>
  filter(term == "weight") |>
  pull(estimate))

# calculate 95% CI by hand
(tidy_m <- tidy_m |>
  mutate(lower = estimate + qt(0.025, df = nrow(d)-2) * std.error,
         upper = estimate + qt(0.975, df = nrow(d) - 2) * std.error))
# last 2 columns should match confint(m) function
confint(m)

# Using do() loop to get a permutation distribution for slope
library(mosaic)
nperm <- 1000
perm <- do(nperm) * {
  d_new <- d
  d_new$weight <- sample(d_new$weight)
  m <- lm(data = d_new, height ~ weight)
  broom::tidy(m) |>
    filter(term == "weight") |>
    pull(estimate)
}
histogram(perm$result)
# calculate se as sd of permutation distribution
(perm.se <- sd(perm$result))
# calculate 95% CI based on permutation distribution
ci <- stats::quantile(c(0.025, 0.975), perm$result)
# visualize
ggplot(data = perm) +
  geom_histogram(aes(x = result)) +
  geom_vline(xintercept = obs_slope, color = "red")
p <- sum(perm$result > abs(obs_slope) | perm$result < -1 * abs(obs_slope))/nperm

# or, using the {infer} workflow...
library(infer)
nperm <- 1000
perm <- d |>
  # specify model
  specify(height ~ weight) |>
  # use a null hypothesis of independence
  hypothesize(null = "independence") |>
  # generate permutation replicates
  generate(reps = nperm, type = "permute") |>
  # calculate the slope statistic
  calculate(stat = "slope")
# calculate se as sd of permutation distribution
perm.se <- sd(perm$stat)
# visualize
visualize(perm) + shade_p_value(obs_stat = obs_slope, direction = "two_sided")

