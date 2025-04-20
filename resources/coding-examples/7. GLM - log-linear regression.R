# GLM - log-linear (Poisson) regression
library(tidyverse)
library(lmtest)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/woollydata.csv"

d <- read_csv(f, col_names = TRUE)
(p <- ggplot(data = d, aes(x = age, y = success)) +
    geom_point())

# glm of success ~ age
m1 <- glm(data = d, success ~ age, family = "poisson")
summary(m1)
(results <- tidy(m1, conf.int = TRUE, conf.level = 0.95))

# is a model with age better than an intercept only model?
m_null <- glm(data = d, success ~ 1, family = "poisson")

# using the `lrtest()` function
lrtest(m_null, m1) # m1 is fuller
Dm_null <- m_null$deviance
Dm1<- m1$deviance
Gsq <- Dm_null - Dm1
(p <- 1 - pchisq(Gsq, df = 1))

# by hand, calculating deviance as -2 * logLik
Dm_null = -2 * logLik(m_null)
Dm1 = -2 * logLik(m1)
Gsq <- as.numeric(Dm_null - Dm1)
(p <- 1 - pchisq(Gsq, df = 1))

# Does adding in rank improve a model over one that just includes sex?

m2 <- glm(data = d, success ~ age + rank, family = "poisson")
summary(m2)

# Does adding in rank improve a model over one that just includes age?

lrtest(m1, m2) # m2 is fuller
Gsq <- m1$deviance - m2$deviance
p <- 1 - pchisq(Gsq, df = 2) # df = 2 because rank has 3 levels thus adds two more coefficients over age alone
