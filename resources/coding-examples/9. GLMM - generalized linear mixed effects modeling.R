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
# for a tidy table of beta coefficients...
broom.mixed::tidy(glmer)

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
broom.mixed::tidy(tmb)

# using {MASS}
library(MASS)
pql <- glmmPQL(
  Shared ~ BegRec +
    offset(log(trialduration/60)),
  random = list(ID = ~1, trial = ~1),
  data = d,
  family = poisson(link = "log"))
summary(pql)
broom.mixed::tidy(pql)

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
summary(admb)
broom.mixed::tidy(admb)

library(sjPlot)
plot_summs(glmer, tmb, pql, admb, model.names = list( "glmer", "tmb", "pql", "adbm"))

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
broom.mixed::tidy(brm)

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
broom.mixed::tidy(stan)

# using {MCMCglmm}
library(MCMCglmm)
prior <- list(R = list(V = 1, nu = 0.002),
              G = list(G1 = list(V = 1e+08, fix = 1)))
mcmc <- MCMCglmm(
  Shared ~ BegRec + log(trialduration/60),
  random = ~ ID + trial + obs,
  nitt = 20000,
  thin = 1,
  data = d,
  family = "poisson",
  verbose = TRUE)
summary(mcmc)
broom.mixed::tidy(mcmc)


lattice::xyplot(mcmc$Sol)
plotMCMC::plotTrace(mcmc$Sol)
lattice::xyplot(mcmc$VCV)
plotMCMC::plotTrace(mcmc$VCV)
lattice::densityplot(mcmc$Sol)
plotMCMC::plotDens(mcmc$Sol)
lattice::densityplot(mcmc$VCV)
plotMCMC::plotDens(mcmc$VCV)

library(bayesplot)
posterior <- as.array(brm)

mixedup::extract_fixed_effects(glmer)
mixedup::extract_fixed_effects(brm)
mixedup::extract_fixed_effects(stan)
conditional_effects(brm)
pp_check(brm)
pp_check(stan)

