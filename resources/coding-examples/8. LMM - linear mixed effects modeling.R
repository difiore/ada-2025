# LMM - mixed effects modeling
library(tidyverse)
library(lme4)
library(lmtest)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/chimpgrooming.csv"
d <- read_csv(f, col_names = TRUE)

ggplot(data = d, aes(y = duration, x = subject)) +
  geom_boxplot()
ggplot(data = d, aes(y = duration, x = reprocondition, fill = parity)) +
  geom_boxplot()
ggplot(data = d, aes(y = duration, x = reprocondition, fill = subject)) +
  geom_boxplot()

# random intercept (aka parallel slopes)
m <- lmer(data = d, duration ~ reprocondition + parity + (1|subject))
summary(m)
coefficients(m)

fuller <- lmer(data=d, duration ~ reprocondition + parity + (1|subject), REML=FALSE)
reduced <- lmer(data=d, duration ~ parity + (1|subject), REML=FALSE)

lrtest(reduced, fuller)
anova(reduced, fuller, test = "Chisq")

# random slopes
# full model with both fixed effects
full <- lmer(data = d,
             duration ~
               reprocondition +
               parity +
               (1 + reprocondition|subject) +
               (1 + parity|subject),
             REML = FALSE)

coefficients(full)

# https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models
# "When you obtain a singular fit, this is often indicating that the model is overfitted – that is, the random effects structure is too complex to be supported by the data, which naturally leads to the advice to remove the most complex part of the random effects structure (usually random slopes). The benefit of this approach is that it leads to a more parsimonious model that is not over-fitted."

# model without reproductive condition
minusRC <- lmer(data = d,
                duration ~
                  parity +
                  (1 + reprocondition|subject) +
                  (1 + parity|subject),
                REML = FALSE)

# model without parity
minusP <- lmer(data = d,
               duration ~ reprocondition +
                 (1 + reprocondition|subject) +
                 (1 + parity|subject),
               REML = FALSE)

# p value for reproductive condition
anova(minusRC, full, test = "Chisq")

# p value for parity
anova(minusP, full, test = "Chisq")

# random factors only
null <- lmer(data = d,
             duration ~
               (1 + reprocondition | subject) +
               (1 + parity | subject),
             REML = FALSE)

# to print table of models by AICc
library(AICcmodavg)
aictab(list(full, minusRC, minusP, null),
       modnames = c("full", "minusRC", "minusP", "null"))

# note that we can also use the {lmerTest} package's version of `lmer()` and it returns p values for each fixed effect
full <- lmerTest::lmer(data = d,
                       duration ~
                         reprocondition +
                         parity +
                         (1 + reprocondition|subject) +
                         (1 + parity|subject),
                       REML = FALSE)
summary(full)

# calculate a coefficient of determination for a mixed model
library(MuMIn)
r.squaredGLMM(full)
