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

boot <- vector()
for (i in 1: n_boot){
  boot[[i]] <- mean(sample(males$kernel95, length(males$kernel95), replace = TRUE))
}

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


ci_f <- mean_f$mean + qnorm(c(0.025, 0.975)) * se_f

se_f <- sd(females$kernel95)/sqrt(length(females$kernel95))











p <- ggplot(data = boot) +
  geom_histogram(aes(x = mean, after_stat(density), color = sex)) +
  stat_function(fun = dnorm,
                args = list(mean = m_mean,
                            sd = m_sd)) +
  stat_function(fun = dnorm,
                args = list(mean = f_mean,
                            sd = f_sd))

t_stat_num <- (mean_m$mean - mean_f$mean) - 0


s2 <- ((nrow(males) - 1) * sd(males$kernel95)^2 + (nrow(females) - 1) * sd(females$kernel95)^2)/(nrow(males) + nrow(females) - 2)

t_stat_denom <- sqrt(s2 * (1/nrow(males) + 1/nrow(females)))


t_stat <- t_stat_num/t_stat_denom

t.test(x = males$kernel95, y = females$kernel95, alternative = "two.sided")




df <- nrow(males) + nrow(females) - 2

l <- pt(-1 * abs(t_stat), df, lower.tail = TRUE)
u <- pt(1 * abs(t_stat), df, lower.tail = FALSE)
p <- l + u

t.test(x = males$kernel95, y = females$kernel95, alternative = "two.sided", var.equal = TRUE)

# Permutation Test
d <- d |>
  select(id, sex, kernel95)

summary <- d |>
  group_by(sex) |>
  summarize(mean = mean(kernel95))

obs <- filter(summary, sex == "F") |> pull(mean) -
       filter(summary, sex == "M") |> pull(mean)

reps <- 10000
perm <- vector()

for (i in 1:reps){
  temp <- d
  temp$sex <- sample(temp$sex)
  summary <- temp |>
    group_by(sex) |>
    summarize(mean = mean(kernel95))
  perm[[i]] <- filter(summary, sex == "F") |> pull(mean) -
    filter(summary, sex == "M") |> pull(mean)
}

hist(perm)

p <- sum(perm < -1 * abs(obs) | perm > abs(obs))/reps

library(infer)
d <- d |> specify(formula = kernel95 ~ sex)
d <- d |> hypothesize(null = "independence")
perm <- d |> generate(reps = 10000, type = "permute")
perm <- perm |> calculate(stat = "diff in means", order = c("M", "F"))
perm
visualize(perm, bins = 20)
obs <- d |>
  specify(kernel95 ~ sex) |>
  calculate(stat = "diff in means", order = c("F", "M"))
visualize(perm, bins = 20) +
  shade_p_value(obs_stat = obs, direction = "both")
get_p_value(perm, obs, direction = "both")
