f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/woolly-weights.csv"
d <- read_csv(f, col_names = TRUE)
head(d)
mu <- 7.2
m <- mean(d$weight)
s <- sd(d$weight)
m <- legnth(d$weight)
se <- s/sqrt(length(d$weight))

CI <- m + qt(p = c(0.025, 0.975), ncp = 0, df = n-1) * se
t_stat <- (m - mu)/se
t_stat
p_lower <- pt(-1 * abs(t_stat), df = n - 1)
p_upper <- pt(-1 * abs(t_stat), df = n - 1)
p <- p_lower + p_upper
t.test(x = x, mu = mu, alternative = "two.sided")


library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/tbs-2006-2008-ranges.csv"
d <- read_csv(f, col_names = TRUE)
head(d)
stats <- d |>
  group_by(sex) |>
  summarize(mean = mean(kernel95),
            sd = sd(kernel95),
            se = sd/sqrt(n()))

p <- ggplot(data = d, mapping = aes(x = sex, y = kernel95)) +
  geom_boxplot() +
  geom_point()

b
