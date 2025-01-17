library(tidyverse)
library(broom)
library(lmtest)
library(effects)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/titanic_train.csv"

d <- read_csv(f, col_names = TRUE)
d$Sex <- factor(d$Sex)
d$Pclass <- factor(d$Pclass)
d$Embarked <- factor(d$Embarked)

d <- d %>% select(-c(PassengerId, Name, Ticket, Cabin))
(p <- ggplot() + geom_point(data = d, aes(x = Age, y = Survived), color = "green") +
  xlab("Age") + ylab("Survived") + ggtitle("Pr(Y) versus Age"))
pairs(d)

(t <- group_by(d, Survived, Sex) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = Sex, values_from = n))

(t <- group_by(d, Survived, Pclass) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = Pclass, values_from = n))

m <- glm(Survived ~ Sex, data = d, family="binomial")
summary(m)
x <- data.frame(Sex = c("male","female"))
logOR <- predict(m, newdata = x)
OR <- exp(logOR)
y <- predict(m, newdata = x, type = "response")
results <- tibble(Sex = c("male","female"), logOR = logOR, OR = OR, Pr_Survival = y)
head(results)




m <- glm(Survived ~ Age, data = d, family="binomial")
summary(m)







x <- data.frame(Age = c(seq(from = min(d$Age, na.rm = TRUE), to = max(d$Age, na.rm = TRUE), by = 1)))
logOR <- predict(m, newdata = x)
OR <- exp(logOR)
y <- predict(m, newdata = x, type = "response")
results <- tibble(Age = c(seq(from = min(d$Age, na.rm = TRUE), to = max(d$Age, na.rm = TRUE), by = 1)), logOR = logOR, OR = OR, Pr_Survival = y)
head(results)
p <- ggplot() +
  geom_point(data=d, aes(x=Age, y=Survived)) +
  xlab("Age") +
  ylab("Pr(Survival)") +
  ggtitle("Pr(Survival) by Age") +
  geom_line(data = results, aes(x = Age, y = Pr_Survival))
p

CI <- confint(m)


reduced <- glm(Survived ~ Pclass, data = d, family="binomial")
summary(m)
x <- data.frame(Pclass = c("1","2","3"))
logOR <- predict(m, newdata = x)
OR <- exp(logOR)
y <- predict(m, newdata = x, type = "response")
results <- tibble(Pclass = c("1","2","3"), logOR = logOR, OR = OR, Pr_Survival = y)
head(results)


library(effects)
plot(allEffects(m))

proposed <- glm(Survived ~ Sex + Pclass, data = d, family="binomial")
summary(m)
newdata <- data.frame(Sex= "female", Pclass="2")

newdata <- data.frame(Sex= "male", Pclass="3")


pred_y <- predict(m, newdata = newdata, type="response")
logOR <- predict(m, newdata=newdata)
pred_Y <- predict(m, newdata=newdata, type="response")
OR <- exp(logOR)


plot(allEffects(m))

m <- glm(Survived ~ Sex * Pclass, data = d, family="binomial")
summary(m)
plot(allEffects(m))

f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/woollydata.csv"
d <- read_csv(f, col_names = TRUE)
(p <- ggplot(data = d, aes(x = age, y = success)) + geom_point() + xlab("Age") +
    ylab("Mating Success"))
# glm of success ~ age
glm <- glm(data = d, success ~ age, family = "poisson")
summary(glm)
results <- tidy(glm, conf.int = TRUE, conf.level = 0.95)

x <- data.frame(age = seq(from = 5, to = 17, by = 1))
prediction <- cbind(age = x, predict(glm, newdata = x, type = "response", se.fit = TRUE))
prediction
alpha <- 0.05
prediction$lower <- prediction$fit - qnorm(1 - alpha/2) * prediction$se.fit
prediction$upper <- prediction$fit + qnorm(1 - alpha/2) * prediction$se.fit
head(prediction)
p <- p + geom_line(data = prediction, aes(x = age, y = fit)) + geom_ribbon(data = prediction, aes(x = age, y = fit, ymin = lower, ymax = upper), alpha = 0.2)
p

# lr test

proposed <- glm(data = d, success ~ age, family = "poisson")
reduced <- glm(data = d, success ~ 1, family = "poisson")
# using the `lrtest()` function
lrtest(reduced, proposed)

D_proposed <- proposed$deviance
D_reduced <- reduced$deviance
x2 <- D_reduced - D_proposed
(p <- 1 - pchisq(x2, df = 1))

# by hand, calculating deviance as -2 * logLik
(D_reduced = -2 * logLik(reduced))
(D_proposed = -2 * logLik(proposed))
(x2 <- D_reduced - D_proposed)
(p <- 1 - pchisq(x2, df = 1))


# mixed effects

library(lmtest)
library(tidyverse)
library(lme4)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/chimpgrooming.csv"
d <- read_csv(f, col_names = TRUE)

# random intercept
m <- lmer(data = d, duration ~ reprocondition + parity + (1 | subject))
summary(m)
vcoefficients(m)
full <- lmer(data = d, duration ~ reprocondition + parity + (1 | subject), REML = FALSE)
summary(full)
reduced <- lmer(data=d, duration~parity+(1|subject), REML=FALSE)
summary(reduced)
lrtest(full, reduced)


# random slope
m <- lmer(data = d, duration ~ reprocondition + parity +
            (1 + reprocondition | subject) +
            (1 + parity | subject), REML = FALSE)
summary(m)
coefficients(m)

# random factors only
null <- lmer(data = d, duration ~ (1 + reprocondition | subject) + (1 + parity |
                                                                      subject), REML = FALSE)

# full model with both fixed effects
full <- lmer(data = d, duration ~ reprocondition + parity +
              (1 + reprocondition | subject) +
              (1 + parity | subject), REML = FALSE)

# model without reproductive condition
minusRC <- lmer(data = d, duration ~ parity +
              (1 + reprocondition | subject) +
              (1 + parity | subject), REML = FALSE)

# model without parity
minusP <- lmer(data = d, duration ~ reprocondition +
              (1 + reprocondition | subject) +
              (1 + parity | subject), REML = FALSE)

# p value for reproductive condition
lrtest(minusRC, full)
anova(minusRC, full, test = "Chisq")

library(AICcmodavg)
aic_table <- aictab(list(full, minusRC, minusP, null),
                    modnames = c("full", "minusRC", "minusP", "null"))
