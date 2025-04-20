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

# GLMM - generalized linear mixed models
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Bowden-ParryOtterdata.csv"
d <- read_csv(f, col_names = TRUE)
d <- d |>
  mutate(trial = paste0(zoo, trialorder)) |>
  rename(Shared = TotalSharebothharasspro) |>
  rename(BegRec = begreceived)

d <- d |>
  rowid_to_column() |>
  mutate(obs = rowid)

d <- d |>
  mutate(ID = factor(ID),
         trial = factor(trial),
         obs = factor(obs))

class(d)

# MCMCglmm (and maybe other packages?) expects the data frame to have "data.frame" as the only class, so we need to convert tibble to data frame
d <- as.data.frame(d)

# using {lme4}
glmer <- glmer(
  Shared ~ BegRec + offset(log(trialduration/60)) +
  (1|ID) + (1|trial) + (1|obs),
  data = d,
  family = poisson(link = "log"))
summary(glmer)
# get the beta coefficients
glmer@beta # this is an S4 data structure, so it uses `@` for slots

jtools::effect_plot(glmer,
                    pred = BegRec, plot.points = TRUE)

# using {glmmTMB}
library(glmmTMB)
tmb <- glmmTMB(
  Shared ~
    BegRec +
    offset(log(trialduration/60)) +
    (1|ID) +
    (1|trial) +
    (1|obs),
  data = d,
  family = poisson(link = "log"))
summary(tmb)

# using {brms}
library(brms)
brm <- brm(
  Shared ~ BegRec + offset(log(trialduration/60)) +
    (1|ID) + (1|trial) + (1|obs),
  data = d,
  iter = 20000,
  family = poisson(link = "log"))
brm
plot(brm)

library(rstanarm)
stan <- stan_glmer(
  Shared ~ BegRec +
    (1|ID) + (1|trial) + (1|obs),
  offset = log(trialduration/60),
  data = d,
  iter = 20000,
  family = poisson(link = "log"))
stan
library(sjPlot)
plot_model(stan)

library(MCMCglmm)

prior <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1e+08, fix = 1)))

mcmc <- MCMCglmm(
  Shared ~ BegRec + offset(log(trialduration/60)),
  random = ~ ID + trial + obs,
  nitt = 20000,
  thin = 1,
  data = d,
  prior = prior,
  family = "poisson",
  verbose = TRUE)
summary(mcmc)

xyplot(as.mcmc(mcmc$Sol))
xyplot(as.mcmc(mcmc$VCV))
densityplot(as.mcmc(mcmc$Sol))
densityplot(as.mcmc(mcmc$VCV))

library(coda)
library(glmmADMB)
library(coefplot2)
coefplot2(list("glmer" = glmer,
               "MCMCglmm" = mcmc),
          intercept = TRUE,
          legend = TRUE)

admb <- glmmadmb(
  Shared ~ BegRec + offset(log(trialduration/60)) +
(1|ID) + (1|trial) + (1|obs),
  data = d,
  family = "poisson")


admb

library(mixedup)
mixedup::extract_fixed_effects(glmer)
mixedup::extract_fixed_effects(brm)
mixedup::extract_fixed_effects(stan)
conditional_effects(brm)
pp_check(brm)
pp_check(stan)
