library(mosaic)

# Example of various ways of calculating CIs for an estimate of the mean
# 1 random sample from population with a given mean and sd
x <- rnorm(n = 100, mean = 2, sd = 4)
m <- mean(x)
se <- sd(x)/sqrt(length(x))

# expected ci based on normal distribution, where pop mean and sd are known
(ci <- 2 + qnorm(c(0.025, 0.975)) * 4/sqrt(length(x)))

# ci based on resampling lots of times from population
reps <- 10000
s <- do(reps) * mean(rnorm(n = 100, mean = 2, sd = 4))
se <- sd(s$mean)
(ci <- mean(s$mean) + qnorm(c(0.025, 0.975)) * se)

# ci based on 1 sample, assuming sampling distribution is normal
(ci <- m + qnorm(c(0.025, 0.975)) * se)

# ci based on 1 sample, assuming sampling distribution is t distribution
(ci <- m + qt(c(0.025, 0.975), df = n-1)  * se)

# ci based on 1 sample, bootstrapping
n_boot <- 10000
boot <- vector()
for (i in 1: n_boot){
  boot[[i]] <- mean (sample(x, length(x), replace = TRUE))
}
(ci <- quantile(boot, probs = c(0.025, 0.975)))

# or
boot <- do(n_boot) * mean (sample(x, length(x), replace = TRUE))
(ci <- quantile(boot$mean, probs = c(0.025, 0.975)))
