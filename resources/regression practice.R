# Regression first steps
library(tidyverse)
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

# Making an interactive regression line
# first, center the data
d <- mutate(d, centered_height = height - mean(height))
d <- mutate(d, centered_weight = weight - mean(weight))

# create a function called slope.test
slope.test <- function(beta1, data) {
  g <- ggplot(data = data,
              aes(x = centered_weight,
                  y = centered_height))
  g <- g + geom_point()
  g <- g + geom_abline(intercept = 0,
                       slope = beta1,
                       linewidth = 1,
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

(b1 <- cov(d$height, d$weight)/var(d$weight))
(b1 <- (cor(d$weight, d$height) * sd(d$height))/(sd(d$weight)))

# Back to simple linear regression
# grab a tidy table of regression statistics
(tidy_m <- broom::tidy(m))
# get just slope coefficient
(obs_slope <- tidy_m |>
  filter(term == "weight") |>
  pull(estimate))

# estimate 95% CIs around coefficients by hand
(tidy_m <- tidy_m |>
  mutate(lower = estimate + qt(0.025, df = nrow(d)-2) * std.error,
         upper = estimate + qt(0.975, df = nrow(d) - 2) * std.error))
# last 2 columns should match results from confint(m) function
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

# p value - proportion of F distribution that lies between 0 and fratio
pf(q = fratio, df1 = 1, df2 = 998, lower.tail = FALSE)
# or
1 - pf(q = fratio, df1 = 1, df2 = 998)

# F dist with 1 and 998 df
mosaic::plotDist("f", df1 =1, df2=998)

(rsq <- SSR/SSY)
summary(m) # looks at multiple R-squared value
anova(m)

# Practice on UNCORRELATED random data drawn from 2 different normal distributions
new_d <- tibble(
  x = rnorm(1000, mean = 100, sd = 25),
  y = rnorm(1000, mean = 10, sd = 2))
plot(new_d$x, new_d$y)
m <- lm(y ~ x, data = new_d)
summary(m)

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
pf(q = fratio, df1 = 1, df2 = nrow(new_d) - 1 - 1, lower.tail = FALSE)

# With Street et al 2017 data
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Street_et_al_2017.csv"
d <- read_csv(f, col_names = TRUE)
m <- lm(log(ECV) ~ log(Body_mass), d)
B1 <- broom::tidy(m) |> filter(term == "log(Body_mass)") |> pull(estimate)
summary(m)
a <- aov(log(ECV) ~ log(Body_mass), d)
summary(a)
SSY <- sum((m$model$`log(ECV)` - mean(m$model$`log(ECV)`)) ^ 2)
SSX <- sum((m$model$`log(Body_mass)` - mean(m$model$`log(Body_mass)`)) ^ 2)
SSR <- sum((m$fitted.values - mean(m$model$`log(ECV)`)) ^ 2)
SSE <- sum((m$model$`log(ECV)` - m$fitted.values) ^ 2)
df_y <- nrow(m$model) - 1
df_r <- 1
df_e <- nrow(m$model) - df_r - 1
(MSY <- SSY/df_y)
(MSR <- SSR/df_r)
(MSE <- SSE/df_e)
(fratio <- MSR/MSE)
(pf(q = fratio, df1 = df_r, df2 = df_e, lower.tail = FALSE))
(SEB1 <- sqrt(MSE/SSX))
(t <- B1/SEB1)
(p <- 2 * pt(abs(t), df = df_e, lower.tail = FALSE))

par(mfrow = c(1, 1))
car::qqPlot(m, distribution = "norm")
ggpubr::ggqqplot(m$residuals)
(s <- shapiro.test(m$residuals))

library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/KamilarAndCooperData.csv"
d <- read_csv(f, col_names = TRUE)
plot(d$Body_mass_female_mean, d$MaxLongevity_m)
plot(log(d$Body_mass_female_mean, log(d$MaxLongevity_m)))
m1 <- lm(MaxLongevity_m ~ Body_mass_female_mean, d)
m2 <- lm(MaxLongevity_m ~ log(Body_mass_female_mean), d)
m3 <- lm(log(MaxLongevity_m) ~ log(Body_mass_female_mean), d)

car::qqPlot(m1)
car::qqPlot(m2)
car::qqPlot(m3)

par(mfrow = c(2, 2))
plot(m1)
plot(m2)
plot(m3)

(shapiro.test(m1$residuals))
(shapiro.test(m2$residuals))
(shapiro.test(m3$residuals))

library(tidyverse)
library(car)
library(jtools)

# ANOVA
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/AVONETdataset1.csv"
d <- read_csv(f, col_names = TRUE)
d <- d |> select(
  Species1, Family1, Order1,
  Beak.Width, Beak.Depth, Tarsus.Length,
  Wing.Length, Tail.Length, Mass,
  Habitat, Migration, Trophic.Level,
  Trophic.Niche, Min.Latitude, Max.Latitude,
  Centroid.Latitude, Range.Size)
glimpse(d)

table(d$Trophic.Niche)
xtabs(~ Trophic.Niche, data = d)
xtabs(~ Habitat + Trophic.Level, data = d)

ggplot(data = d |> drop_na(Habitat),
       aes(x = Habitat, y = log(Mass))) +
  geom_boxplot() +
  geom_jitter()

ggplot(data = d |> drop_na(Trophic.Level),
       aes(x = Trophic.Level, y = log(Mass))) +
  geom_boxplot() +
  geom_jitter()

d <- d |> mutate(Migration = as.factor(Migration))

m1 <- lm(log(Mass) ~ Trophic.Level, data = d)
m2 <- lm(log(Mass) ~ Migration, data = d)
summary(m1)
summary(m1)$fstatistic
summary(m2)
summary(m2)$fstatistic

pf(summary(m1)$fstatistic[1], df1 = summary(m1)$fstatistic[2], df2=summary(m1)$fstatistic[3], lower.tail = FALSE)

pf(summary(m2)$fstatistic[1], df1 = summary(m2)$fstatistic[2], df2=summary(m2)$fstatistic[3], lower.tail = FALSE)

mosaic::plotDist("f", df1 = summary(m1)$fstatistic[2], df2=summary(m1)$fstatistic[3])

d <- d |> mutate(Trophic.Level = relevel(as.factor(Trophic.Level), ref = "Omnivore"))

(pairwise.t.test(log(d$Mass), d$Trophic.Level, p.adj = "bonferroni"))

# extract aov table from the model either by running aov() on the original model or on the output of lm()
m1aov <- aov(log(Mass) ~ Trophic.Level, data = d)
# or
m1aov <- aov(m1)

# note: posthoc Tukey test can be run on either m1 or m1aov
(posthoc <- TukeyHSD(m1,
                     which = "Trophic.Level",
                     conf.level = 0.95))

# permutation approach to anova... compare original f statistic to permutation distribution
original.F <- summary(m1)$fstatistic[1]

library(infer)
d <- d |> mutate(logMass = log(Mass))
# create a variable that is the log of body mass because cannot call specify() with an operation on the LHS of a formula
permuted.F <- d |>
  specify(logMass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")

visualize(permuted.F) +
  shade_p_value(obs_stat = 	original.F,
                direction = "greater")

p.value <- permuted.F |>
  get_p_value(obs_stat = original.F,
              direction = "greater")

# Multiple Regression/ANCOVA
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z <- read_csv(f, col_names = TRUE)
m <- lm(height ~ weight + age, data = z)

summary(m)
plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
plot(fitted(m), residuals(m))
summary(aov(m))

(f <- (summary(m)$r.squared*(nrow(z)-2-1))/((1-summary(m)$r.squared) * 2))

(p <- pf(f, df1 = 2, df2 = 997, lower.tail = FALSE))
(p <- pf(f, df1 = summary(m)$fstatistic[2],
         df2 = summary(m)$fstatistic[3],
         lower.tail = FALSE))

m <- lm(height ~ weight + age + gender , data = z)
summary(m)

plot(m$model$weight, residuals(m))
plot(m$model$age, residuals(m))
boxplot(residuals(m) ~ m$model$gender)
plot(fitted(m), residuals(m))
library(car)
vif(m)

temp <- lm(weight ~ age + gender, data = z)
s <- summary(temp)
1/(1-s$r.squared)

# Confidence and Prediction Intervals
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

# Effect Plots
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

# GLM
library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/titanic_train.csv"
d <- read_csv(f, col_names = TRUE)
d <- d |> select(-c(PassengerId, Ticket, Cabin))
d <- d |> mutate(Sex = factor(Sex),
                 Pclass = factor(Pclass),
                 Embarked = factor(Embarked))
(p1 <- ggplot(data = d, aes(x = Age, y = Survived)) +
    geom_point())
(p2 <- ggplot(data = d, aes(x = Sex, y = Survived)) +
    geom_violin() +
    geom_jitter())
(p3 <- ggplot(data = d, aes(x = Pclass, y = Survived)) +
    geom_violin() +
    geom_jitter())

m <- glm(Survived ~ Sex, data = d, family = "binomial")
summary(m)

coefs <- broom::tidy(m) |> select(estimate)
logOR_female_survival <- coefs$estimate[1] + coefs$estimate[2] * 0
logOR_male_survival <- coefs$estimate[1] + coefs$estimate[2] * 1

OR_female_survival <- exp(logOR_female_survival) # odds of females surviving versus not
OR_male_survival <- exp(logOR_male_survival) # odds of males surviving versus not

PR_male_survival <- OR_male_survival/(1 + OR_male_survival)
PR_female_survival <- OR_female_survival/(1 + OR_female_survival)

table(d$Survived, d$Pclass)

x <- data.frame(Sex = c("male","female"))
logOR <- predict(m, newdata = x)
OR <- exp(logOR)

logOR <- predict(m, newdata = x, type = "response", se.fit = TRUE)

m <- glm(Survived ~ Age, data = d, family = "binomial")

summary(m)
coef(m)
coef <- broom::tidy(m)
coef$estimate[2]
exp(coef$estimate[2])

# How is survival related to passenger class (Pclass)? What is the equation that results from the model? # Is survival significantly associated with passenger class?

# Yes!

# How is survival related to both Sex and Pclass? What is the equation that results from the model? What are the predicted odds of survival for a second class woman traveler? What is the estimated probability of survival for a male in third class?



m1 <- glm(Survived ~ Pclass, data = d, family = "binomial")
summary(m1)

coefs <- broom::tidy(m1) |> select(estimate)

logOR_class1 <- coefs$estimate[1] + coefs$estimate[2] * 0 + coefs$estimate[3] * 0

logOR_class2 <- coefs$estimate[1] + coefs$estimate[2] * 1 + coefs$estimate[3] * 0

logOR_class3 <- coefs$estimate[1] + coefs$estimate[2] * 0 + coefs$estimate[3] * 1

x <- data.frame(Pclass = c("1", "2", "3"))
logOR <- predict(m1, newdata = x)

# What is the predicted odds of survival for a traveler in first class? What is the estimated probability of survival for a first class passenger?

OR <- exp(logOR)

PrS_C1 <- OR[1]/(1 + OR[1])
PrS_C2 <- OR[2]/(1 + OR[2])
PrS_C3 <- OR[3]/(1 + OR[3])

PrS <- predict(m1, newdata = x, type = "response")

# How is survival related to both Sex and Pclass? What is the equation that results from the model? What are the predicted odds of survival for a second class woman traveler? What is the estimated probability of survival for a male in third class?


m2 <- glm(Survived ~ Sex + Pclass, data = d, family = "binomial")
summary(m2)



x <- data.frame(Sex = c("male", "female", "male", "female", "male", "female"), Pclass = c("1", "1",
"2", "2", "3", "3"))

PrS <- predict(m2, newdata = x, type = "response")





x <- data.frame(Sex = c("male"), Pclass = c("3"))
OR <- predict(m2, newdata = x)
PrS <- predict(m2, newdata = x, type = "response")

library(lmtest)
lrtest(m1, m2)


anova(m1, m2, test = "Chisq")




Gsq <- m1$deviance - m2$deviance
p <- 1 - pchisq(Gsq, df = 1)
# where df = difference in number of parameters in the proposed versus reduced model

# log-linear
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/woollydata.csv"

d <- read_csv(f, col_names = TRUE)
d
p <- ggplot(data = d, aes(x = age, y = success)) +
  geom_point()

m1 <- glm(data =d, success ~ age + rank, family = "poisson")
summary(m1)

m2 <- glm(data =d, success ~ age, family = "poisson")
summary(m2)


lrtest(m1)








# GLMM
library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/chimpgrooming.csv"

d <- read_csv(f, col_names = TRUE)

ggplot(data = d, aes(y = duration, x = subject)) +
  geom_boxplot()
ggplot(data = d, aes(y = duration, x = reprocondition, fill = parity)) +
  geom_boxplot()
ggplot(data = d, aes(y = duration, x = reprocondition, fill = subject)) +
  geom_boxplot()

library(lme4)
m <- lmer(data = d, duration ~ reprocondition + parity + (1|subject))

summary(m)

fuller <- lmer(data=d, duration ~ reprocondition + parity + (1|subject), REML=FALSE)

reduced <- lmer(data=d, duration ~ parity + (1|subject), REML=FALSE)

anova(reduced, fuller, test = "Chisq")



full <- lme4::lmer(data = d, duration ~ reprocondition + parity +
            (1 + reprocondition|subject) +
            (1 + parity|subject), REML = FALSE)

minusRC <- lme4::lmer(data = d, duration ~ parity +
               (1 + reprocondition|subject) +
               (1 + parity|subject), REML = FALSE)

minusP <- lme4::lmer(data = d, duration ~ reprocondition +
                  (1 + reprocondition|subject) +
                  (1 + parity|subject), REML = FALSE)

anova(minusP, full, test = "Chisq")




summary(m)
# https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models
# "When you obtain a singular fit, this is often indicating that the model is overfitted – that is, the random effects structure is too complex to be supported by the data, which naturally leads to the advice to remove the most complex part of the random effects structure (usually random slopes). The benefit of this approach is that it leads to a more parsimonious model that is not over-fitted."


library(lmerTest)
fuller <- lmer(data=d, duration ~ reprocondition + parity + (1|subject), REML=FALSE)

# full model with both fixed effects
full <- lme4::lmer(data = d,
             duration ~
               reprocondition +
               parity +
               (1 + reprocondition|subject) +
               (1 + parity|subject), REML = FALSE)
# model without reproductive condition
minusRC <- lme4::lmer(data = d,
                  duration ~ parity +
                    (1 + reprocondition|subject) +
                    (1 + parity|subject), REML = FALSE)
# model without parity
minusP <- lme4::lmer(data = d,
                 duration ~ reprocondition +
                   (1 + reprocondition|subject) +
                   (1 + parity|subject), REML = FALSE)
# p value for reproductive condition
anova(minusRC, full, test = "Chisq")
# p value for parity
anova(minusP, full, test = "Chisq")

null <- lme4::lmer(data = d,
                   duration ~
                     (1 + reprocondition | subject) +
                     (1 + parity | subject), REML = FALSE)

library(AICcmodavg)
aictab(list(full, minusRC, minusP, null),
       modnames = c("full", "minusRC", "minusP", "null"))



library(MuMIn)
r.squaredGLMM(full)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Bowden-ParryOtterdata.csv"
d <- read_csv(f, col_names = TRUE)
d <- d |>
  mutate(trial = paste0(zoo, trialorder)) |>
  rename(Shared = TotalSharebothharasspro) |>
  rename(BegRec = begreceived)

d <- d |>
  rowid_to_column() |>
  mutate(obs = rowid)

m <- glmer(Shared ~ BegRec +
             offset(log(trialduration/60)) +
                      (1 | ID) +
                      (1 | trial) +
                      (1 | obs),
           data = d,
           family = poisson(link = "log"))
summary(m)




full <- lme4::lmer(data = d,
                   duration ~ reprocondition + parity +
                     (1 | subject), REML = FALSE)


minusRC <- lme4::lmer(data = d,
                   duration ~  parity +
                     (1 | subject), REML = FALSE)

minusP <- lme4::lmer(data = d,
                      duration ~  reprocondition +
                        (1 | subject), REML = FALSE)
null <- lme4::lmer(data = d, duration ~ (1|subject), REML = FALSE)
