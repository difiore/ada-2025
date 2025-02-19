library(mosaic)


















library(mosaic)

mu <-10 # for "mean" parameter!
sigma <- 2 # for "sd" parameter!
plotDist("norm", mean=mu, sd=sigma, xlab="x", ylab="Frequency")

s1 <- rnorm(n = 10, mean = 10, sd = 2)
mean(s1)
sd(s1)









s2 <- rpois(n = 10, lambda = 10)
s2

mean(s1)










sd(s1)

s2 <- rnorm(n = 1000, mean = 10, sd = 2)
mean(s2)
sd(s2)

histogram(s2)




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




plotDist("t", df = 99, xlab="x", ylab="Frequency", col = "red")
plotDist("t", df = 50, add = TRUE)
plotDist("t", df = 25, add = TRUE)
plotDist("t", df = 12, add = TRUE)
plotDist("t", df = 6, add = TRUE)
plotDist("t", df = 3, add = TRUE)
plotDist("t", df = 1, add = TRUE, col = "green")

plotDist("t", df=50, xlab="x", ylab="Frequency", col="red")
plotDist("norm", mu=0, sd=1, add=TRUE)


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

x <- rbeta(n = 1000, shape1 = 0.3, shape2 = 4)
histogram(x)
reps <- 1000
samp_dist_mean <-
  do(reps) * mean(rbeta(n = 1000, shape1 = 0.3, shape2 = 4))
histogram(~ mean, data = samp_dist_mean, xlab = "Samp Dist for the Mean", ylab = "Frequency")




plotDist("beta", shape1 = .3, shape2 = 4)
reps <- 1000
s <- do(reps) * mean(rbeta(n=100, shape1 = .3, shape2 = 4))
histogram(s$mean)


