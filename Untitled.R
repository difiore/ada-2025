library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/woolly-weights.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

mu <- 7.2
m <- mean(d$weight)
s <- sd(d$weight)
n <- length(d$weight)
se <- s/sqrt(n)

(CI_t <- m + qt(p = c(0.025, 0.975), ncp = 0, df = n-1) * se)
(CI_norm <- m + qnorm(p = c(0.025, 0.975)) * se)

n_boot <- 10000
boot <- vector()
for (i in 1: n_boot){
  boot[[i]] <- mean(sample(d$weight, nrow(d), replace = TRUE))
}
head(boot)
hist(boot)
CI <- quantile(probs = c(0.025, 0.975), boot)

t_stat <- (m - mu)/se
t_stat
(p_lower <- pt(-1 * abs(t_stat), df = n - 1))
(p_upper <- 1 - pt(1 * abs(t_stat), df = n - 1))
(p <- p_lower + p_upper)

t.test(x = d$weight, mu = mu, alternative = "two.sided")

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/tbs-2006-2008-ranges.csv"
d <- read_csv(f, col_names = TRUE)
head(d)

stats <- d |>
  group_by(sex) |>
  summarise(mk95 = mean(kernel95),
            sdk95 = sd(kernel95),
            sek95 = sdk95/sqrt(n()))

stats <- d |>
  group_by(sex) |>
  summarize(mean = mean(kernel95),
            sd = sd(kernel95),
            se = sd/sqrt(n()))

(p <- ggplot(data = d, mapping = aes(x = sex, y = kernel95)) +
  geom_boxplot() +
  geom_point())

males <- d |>
  filter(sex == "M")

females <- d |>
  filter(sex == "F")

(mean_m <- males |>
  summarize(mean = mean(kernel95)))

(mean_f <- females |>
  summarize(mean = mean(kernel95)))

library(mosaic)
n_boot <- 10000

boot_m <- do(n_boot) * mean (sample(males$kernel95, length(males$kernel95), replace = TRUE))
(ci_m <- quantile(boot_m$mean, probs = c(0.025, 0.975)))
histogram(boot_m$mean)
m_mean <- mean(boot_m$mean)
m_sd <- sd(boot_m$mean)
plotDist("norm", mean = m_mean, sd = m_sd, add = TRUE)

(
  p <- ggplot(data = boot_m) +
  geom_histogram(aes(x = mean, after_stat(density))) +
  stat_function(fun = dnorm,
                args = list(mean = m_mean,
                            sd = m_sd))
)

boot_f <- do(n_boot) * mean (sample(females$kernel95, length(females$kernel95), replace = TRUE))
(ci_f <- quantile(boot_f$mean, probs = c(0.025, 0.975)))

histogram(boot_f$mean)
f_mean <- mean(boot_f$mean)
f_sd <- sd(boot_f$mean)
plotDist("norm", mean = f_mean, sd = f_sd, add = TRUE)

boot_m$sex <- "male"
boot_f$sex <- "female"
boot <- bind_rows(boot_m, boot_f)

p <- ggplot(data = boot) +
  geom_histogram(aes(x = mean, after_stat(density), color = sex)) +
  stat_function(fun = dnorm,
                args = list(mean = m_mean,
                            sd = m_sd)) +
  stat_function(fun = dnorm,
                args = list(mean = f_mean,
                            sd = f_sd))
