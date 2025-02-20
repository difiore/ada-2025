library(mosaic)


reps <- 500

samp_dist_mean <-
  do(reps) * mean(rnorm(n = 10, mean = 10, sd = 2))

histogram(samp_dist_mean$mean)

str(samp_dist_mean)
# generates a sampling distribution for the mean of our sample

samp_dist_median <-
  do(reps) * median(rnorm(n = 10, mean = 10, sd = 2))
str(samp_dist_median)
# generates a sampling distribution for the median of our sample

var(samp_dist_mean)
sd(samp_dist_mean$mean)

library(plotrix)
x <- rnorm(n = 10, mean = 10, sd = 2)



sd(samp_dist_mean$mean)








std.error(x)


sd(x)/sqrt(length(x))

x <- rnorm(n = 10, mean = 10, sd = 2)


std.error(x)




sd(x)/sqrt(length(x))




reps <- 1000
s <-
  do(reps) * mean(rnorm(n = 100, mean = 2, sd = 4))
(se <- sd(s$mean))
histogram(s$mean)




histogram(~ mean, data = samp_dist_mean, xlab = "Samp Dist for the Mean")
se_mean <- sd(samp_dist_mean$mean)


# t distribution

plotDist("t", df = 99, xlab="x", ylab="Frequency", col = "red")
plotDist("t", df = 50, add = TRUE)
plotDist("t", df = 25, add = TRUE)
plotDist("t", df = 12, add = TRUE)
plotDist("t", df = 6, add = TRUE)
plotDist("t", df = 3, add = TRUE)
plotDist("t", df = 1, add = TRUE, col = "green")


plotDist("t", df=99, ncp = 2, xlab="x", ylab="Frequency", col="red")
x <- rt(n = 100, df = 99, ncp = 2)
mean(x)
sd(x)
sd(x)/sqrt(length(x))

reps <- 1000
samp_dist_mean <-
  do(reps) * mean(rt(n = 100, df = 99, ncp = 2))
histogram(~ mean, data = samp_dist_mean, xlab = "Samp Dist for the Mean", ylab = "Frequency")
str(samp_dist_mean)
sd(samp_dist_mean$mean)

pt(q = 2, df = 99, ncp = 2)
dt(x = 2, df = 99, ncp = 2)

qnorm(p = c(0.025, 0.975), mean = 0, sd = 1)


x1 <- rnorm(n = 1000, mean = 0, sd = 1)
x2 <- rbeta(n = 1000, shape1 = 0.3, shape2 = 4) * 100
x3 <- x1 + x2
histogram(x1)
histogram(x2)
histogram(x3)

reps <- 10000
samp_dist_mean <-
  do(reps) * mean(rnorm(n = 1000, mean = 0, sd = 1) +
                  rbeta(n = 1000, shape1 = 0.3, shape2 = 4)
                  )
histogram(~ mean, data = samp_dist_mean, xlab = "Samp Dist for the Mean", ylab = "Frequency")

qqnorm(samp_dist_mean$mean)
qqline(samp_dist_mean$mean)


plotDist("norm", mean = 0, sd = 1)
abline( h = .2)
abline(v = qnorm(p = c(0.025, 0.975), mean = 0, sd = 1))

library(mosaic)

# Plot a normal distribution
plotDist("norm", mean = 0, sd = 1, col = "blue")
ladd(panel.abline(v = qnorm(c(0.025, 0.975), mean = 0, sd = 1)))

# Plot a beta distribution
plotDist("beta", shape1 = 2, shape2 = 4)
ladd(panel.abline(v = qbeta(p = c(0.025, 0.975), shape1 = 2, shape2 = 4)))

# Plot a beta distribution
plotDist("beta", shape1 = .3, shape2 = 4)
ladd(panel.abline(v = qbeta(p = c(0.025, 0.975), shape1 = 0.3, shape2 = 4)))

x <- rnorm(n = 10, mean = 2, sd = 4)

x <- c(2.9, 4.8, 8.9, -3.2, 9.1, -2.5, -0.9, -0.1, 2.8, -1.7)
m <- mean(x)
se <- sd(x)/sqrt(length(x))

ci <- m + c(-1,1) * qnorm(c(0.025, 0.975)) * se


s <- do(reps) * mean(rbeta(n=100, shape1 = .3, shape2 = 4))
histogram(s$mean)





library(manipulate)
manipulate(
  ggplot(data = data.frame(
    x = c(
      sampling_dist_mean - 4 * sampling_dist_sd,
      sampling_dist_mean + 4 * sampling_dist_sd
    )
  ), aes(x)) + stat_function(
    fun = dnorm,
    args = list(mean = sampling_dist_mean, sd = sampling_dist_sd),
    n = 1000
  ) + xlab("Sampling Distribution Mean") +
    ylab("") + labs(
      title = "Exploring Confidence Intervals",
      subtitle = paste0(
        "Sampling Distribution SD (= SE): ",
        sampling_dist_sd,
        "\n",
        round(percent_CI, 2),
        "% CI: ",
        round(sampling_dist_mean -
                qnorm((1 - percent_CI /
                         100) / 2) * sampling_dist_sd, 2),
        " to ",
        round(sampling_dist_mean +
                qnorm((1 - percent_CI /
                         100) / 2) * sampling_dist_sd, 2)
      )
    ) + geom_vline(
      xintercept = sampling_dist_mean -
        qnorm((1 - percent_CI /
                 100) / 2) * sampling_dist_sd,
      color = "blue",
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = sampling_dist_mean + qnorm((1 - percent_CI / 100) / 2) *
        sampling_dist_sd,
      color = "blue",
      linetype = "dashed"
    ) + geom_vline(
      xintercept = sampling_dist_mean,
      color = "black",
      linetype = "solid"
    ) + stat_function(
      fun = dnorm,
      xlim = c(
        sampling_dist_mean -
          qnorm((1 - percent_CI /
                   100) / 2) * sampling_dist_sd,
        sampling_dist_mean + qnorm((1 -
                                      percent_CI /
                                      100) / 2) * sampling_dist_sd
      ),
      args = list(mean = sampling_dist_mean, sd = sampling_dist_sd),
      n = 1000,
      geom = "area",
      fill = "red",
      alpha = 0.5,
      color = "red"
    ),
  sampling_dist_mean = slider(-100, 100, initial = 0, step = 10),
  sampling_dist_sd = slider(0, 100, initial = 1, step = 1),
  percent_CI = slider(0, 99, initial = 95, step = 1)
)

