# GLM - binomial regression
library(tidyverse)
library(lmtest)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/titanic_train.csv"
d <- read_csv(f, col_names = TRUE)
d <- d |> select(-c(Name, PassengerId, Ticket, Cabin))
d <- d |> mutate(Sex = factor(Sex),
                 Pclass = factor(Pclass),
                 Embarked = factor(Embarked))
# bivariate scatterplot
pairs(d)
(p1 <- ggplot(data = d, aes(x = Age, y = Survived)) +
    geom_point(color = "blue") +
    xlab("Age") +
    ylab("Survived") +
    ggtitle("Pr(Y) versus Age"))

(p2 <- ggplot(data = d, aes(x = Sex, y = Survived)) +
    geom_violin() +
    geom_jitter())
(p3 <- ggplot(data = d, aes(x = Pclass, y = Survived)) +
    geom_violin() +
    geom_jitter())

# summarize cases

table(d$Survived, d$Sex)
# OR
(t <- group_by(d, Survived, Sex) |>
    summarize(n = n()) |>
    pivot_wider(names_from = Sex, values_from = n))

table(d$Survived, d$Pclass)
# OR
(t <- group_by(d, Survived, Pclass) |>
    summarize(n = n()) %>%
    pivot_wider(names_from = Pclass, values_from = n))

m <- glm(Survived ~ Sex, data = d, family = "binomial")
summary(m)

coefs <- broom::tidy(m) |> select(estimate)
logOR_female_survival <- coefs$estimate[1] + coefs$estimate[2] * 0
logOR_male_survival <- coefs$estimate[1] + coefs$estimate[2] * 1

OR_female_survival <- exp(logOR_female_survival) # odds of females surviving versus not
OR_male_survival <- exp(logOR_male_survival) # odds of males surviving versus not

PR_male_survival <- OR_male_survival/(1 + OR_male_survival)
PR_female_survival <- OR_female_survival/(1 + OR_female_survival)

x <- data.frame(Sex = c("male","female"))
(logOR <- predict(m, newdata = x))
(OR <- exp(logOR))
(PrS <- predict(m, newdata = x, type = "response"))
(results <- tibble(Sex = c("male","female"), logOR = logOR, OR = OR, PrS = PrS))

library(effects)
plot(allEffects(m))

m <- glm(Survived ~ Age, data = d, family = "binomial")

summary(m)
coef(m)
coef <- broom::tidy(m)
coef$estimate[2]
exp(coef$estimate[2])

x <- data.frame(Age = c(seq(from = min(d$Age, na.rm = TRUE), to = max(d$Age, na.rm = TRUE), by = 1)))
logOR <- predict(m, newdata = x)
OR <- exp(logOR)
PrS <- predict(m, newdata = x, type = "response")
results <- tibble(Age = c(seq(from = min(d$Age, na.rm = TRUE), to = max(d$Age, na.rm = TRUE), by = 1)), logOR = logOR, OR = OR, PrS = PrS)
head(results)

p <- ggplot() +
  geom_point(data=d, aes(x = Age, y=Survived), color = "green") +
  xlab("Age") +
  ylab("Pr(Survival)") +
  ggtitle("Pr(Survival) by Age") +
  geom_line(data = results, aes(x = Age, y = PrS))
p

plot(allEffects(m))

# EXAMPLE
# How is survival related to passenger class (Pclass)? Is survival significantly associated with passenger class?

m1 <- glm(Survived ~ Pclass, data = d, family = "binomial")
summary(m1)

# Yes!

# What is/are the equation(s) that results from the model?
coefs <- broom::tidy(m1) |> select(estimate)
logOR_class1 <- coefs$estimate[1] + coefs$estimate[2] * 0 + coefs$estimate[3] * 0

logOR_class2 <- coefs$estimate[1] + coefs$estimate[2] * 1 + coefs$estimate[3] * 0

logOR_class3 <- coefs$estimate[1] + coefs$estimate[2] * 0 + coefs$estimate[3] * 1

# What is the predicted odds of survival for a traveler in first class?

x <- data.frame(Pclass = c("1", "2", "3"))
logOR <- predict(m1, newdata = x)
OR <- exp(logOR)

# What is the estimated probability of survival for a first class passenger?

PrS_C1 <- OR[1]/(1 + OR[1])
PrS_C2 <- OR[2]/(1 + OR[2])
PrS_C3 <- OR[3]/(1 + OR[3])

PrS <- predict(m1, newdata = x, type = "response")

# How is survival related to both Sex and Pclass? What is/are the equation(s) that results from the model?

m2 <- glm(Survived ~ Sex + Pclass, data = d, family = "binomial")
summary(m2)

# What are the predicted odds of survival for a second class woman traveler?

x <- data.frame(Sex = c("female"), Pclass = c("2"))
logOR <- predict(m2, newdata = x)
OR <- exp(logOR)
PrS <- predict(m2, newdata = x, type = "response")

# What is the estimated probability of survival for a male in third class?
x <- data.frame(Sex = c("male"), Pclass = c("3"))
logOR <- predict(m2, newdata = x)
OR <- exp(logOR)
PrS <- predict(m2, newdata = x, type = "response")

# all combinations of Sex and Pclass
x <- data.frame(
  Sex = c("male", "female", "male", "female", "male", "female"),
  Pclass = c("1", "1", "2", "2", "3", "3"))
logOR <- predict(m, newdata = x)
OR <- exp(logOR)
PrS <- predict(m2, newdata = x, type = "response")

results <- tibble(Sex = x$Sex, Pclass = x$Pclass, logOR = logOR, OR = OR, PrS = PrS)
head(results)

plot(allEffects(m2))

# Does adding Sex in to a model with Pclass improve the model's explanatory power?

anova(m1, m2, test = "Chisq") # m2 is fuller
# OR
lrtest(m1, m2) #m2 is fuller

Gsq <- m1$deviance - m2$deviance
p <- 1 - pchisq(Gsq, df = 1)
# where df = difference in number of parameters in the fuller versus reduced model

m <- glm(Survived ~ Sex*Pclass, data = d, family="binomial")
summary(m)
effect("Sex", m)
effect("Pclass", m)
effect("Sex:Pclass", m)
plot(allEffects(m))
