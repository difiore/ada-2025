# GLMM - generalized linear mixed models
library(tidyverse)
library(lme4)
library(lmtest)
library(coda)
library(coefplot2)
library(jtools)
library(mixedup)
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
# to get the beta coefficients...
glmer@beta # this is an S4 data structure, so it uses `@` for slots

glmer_null <- glmer(
  Shared ~ offset(log(trialduration/60)) +
    (1|ID) + (1|trial) + (1|obs),
  data = d,
  family = poisson(link = "log"))
summary(glmer_null)

lrtest(glmer_null, glmer)

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

# using {MASS}
library(MASS)
pql <- glmmPQL(
  Shared ~ BegRec +
    offset(log(trialduration/60)),
  random = list(ID = ~1, trial = ~1),
  data = d,
  family = poisson(link = "log"))

library(sjPlot)
plot_summs(glmer, tmb, pql)

# using {brms} and STAN
library(brms)
brm <- brm(
  Shared ~ BegRec +
    offset(log(trialduration/60)) +
    (1|ID) +
    (1|trial) +
    (1|obs),
  data = d,
  iter = 20000,
  family = poisson(link = "log"))
brm

# using {rstanarm} and STAN
library(rstanarm)
stan <- stan_glmer(
  Shared ~ BegRec +
    (1|ID) +
    (1|trial) +
    (1|obs),
  offset = log(trialduration/60),
  data = d,
  iter = 20000,
  family = poisson(link = "log"))
stan

# using {MCMCglmm}
library(MCMCglmm)
prior <- list(R = list(V = 1, nu = 0.002),
              G = list(G1 = list(V = 1e+08, fix = 1)))
mcmc <- MCMCglmm(
  Shared ~ BegRec + offset(log(trialduration/60)),
  random = ~ ID + trial + obs,
  nitt = 20000,
  thin = 1,
  data = d,
  family = "poisson",
  verbose = TRUE)
summary(mcmc)

lattice::xyplot(as.mcmc(mcmc$Sol))
lattice::xyplot(as.mcmc(mcmc$VCV))
lattice::densityplot(as.mcmc(mcmc$Sol))
lattice::densityplot(as.mcmc(mcmc$VCV))

library(bayesplot)
posterior <- as.array(brm)
mcmc_scatter(brm)
mcmc_intervals(posterior)

# using {glmmADMB}
library(glmmADMB)

admb <- glmmadmb(
  Shared ~ BegRec +
    offset(log(trialduration/60)) +
    (1|ID) +
    (1|trial) +
    (1|obs),
  data = d,
  family = "poisson")

admb


mixedup::extract_fixed_effects(glmer)
mixedup::extract_fixed_effects(brm)
mixedup::extract_fixed_effects(stan)
conditional_effects(brm)
pp_check(brm)
pp_check(stan)
