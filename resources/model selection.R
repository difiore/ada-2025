library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/AVONETdataset1.csv"

d <- read_csv(f, col_names = TRUE)
d <- d %>% select(Species1, Family1, Order1, Beak.Length_Culmen, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level, Trophic.Niche, Min.Latitude, Max.Latitude, Centroid.Latitude, Range.Size, Primary.Lifestyle)

d <- d %>% mutate(logMass = log(Mass),
                  logRS = log(Range.Size),
                  logBeak = log(Beak.Length_Culmen),
                  logTarsus = log(Tarsus.Length),
                  Migration = as.factor(Migration))

relBeak <- lm(logBeak ~ logMass, data = d, na.action=na.exclude)
relTarsus <- lm(logTarsus ~ logMass, data = d, na.action=na.exclude)
d <- d %>% mutate(
  relBeak = residuals(relBeak),
  relTarsus = residuals(relTarsus))

m1 <- lm(data = d, logBeak ~ logRS * Migration) # full model
m2 <- lm(data = d, logBeak ~ logRS + Migration) # model without interaction
m3 <- lm(data = d, logBeak ~ logRS) # model with one predictor
m4 <- lm(data = d, logBeak ~ Migration) # model with one predictor
m5 <- lm(data = d, logBeak ~ 1) # intercept only model

anova(m2, m1, test ="F")
anova(m3, m1, test = "F")

d_new <- d %>% drop_na(logRS, Migration)
m1 <- lm(data = d_new, logBeak ~ logRS * Migration) # full model
m2 <- lm(data = d_new, logBeak ~ logRS + Migration) # model without interaction
m3 <- lm(data = d_new, logBeak ~ logRS) # model with one predictor
m4 <- lm(data = d_new, logBeak ~ Migration) # model with one predictor
m5 <- lm(data = d_new, logBeak ~ 1) # intercept only model

anova(m2, m1, test = "F")
anova(m3, m2, test = "F")
anova(m4, m2, test = "F")

# add trophic level


library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/AVONETdataset1.csv"

d <- read_csv(f, col_names = TRUE)
d <- d %>% select(Species1, Family1, Order1, Beak.Length_Culmen, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level, Trophic.Niche, Min.Latitude, Max.Latitude, Centroid.Latitude, Range.Size, Primary.Lifestyle)

d <- d %>% mutate(logMass = log(Mass),
                  logRS = log(Range.Size),
                  logBeak = log(Beak.Length_Culmen),
                  logTarsus = log(Tarsus.Length),
                  Migration = as.factor(Migration))

relBeak <- lm(logBeak ~ logMass, data = d)
relTarsus <- lm(logTarsus ~ logMass, data = d)
d <- d %>% mutate(
  relBeak = relBeak$residuals,
  relTarsus = relTarsus$residuals)

d_new <- d %>% drop_na(logRS, logTarsus, Migration, Trophic.Level,  Primary.Lifestyle)

m_full <- lm(data = d_new, relBeak ~ logRS + logTarsus + Migration + Trophic.Level + Primary.Lifestyle) # full model
m2 <- lm(data = d_new, relBeak ~ logRS + Trophic.Level) # reduced model
m3 <- lm(data = d_new, relBeak ~ Migration + Trophic.Level) # reduced model
m4 <- lm(data = d_new, relBeak ~ Migration + logRS) # reduced model
m5 <- lm(data = d_new, relBeak ~ logRS) # reduced model
m6 <- lm(data = d_new, relBeak ~ Migration) # reduced model
m7 <- lm(data = d_new, relBeak ~ Trophic.Level) # reduced model
m_null <- lm(data = d_new, relBeak ~ 1) # intercept only model

anova(m2, m_full, test = "F")

f <- ((summary(m_full)$r.squared - summary(m2)$r.squared) * (nrow(d_new) - 11 - 1)) / ((1 - summary(m_full)$r.squared) * (11 - 4))

# forward
add1(m_null, scope = . ~ . + logRS + logTarsus + Migration + Trophic.Level + Primary.Lifestyle, test = "F")
m1 <- update(m_null, formula = . ~ . + Primary.Lifestyle)
add1(m1, scope = . ~ . + logRS + logTarsus + Migration + Trophic.Level, test = "F")
m2 <- update(m1, formula = . ~ . + Trophic.Level)
add1(m2, scope = . ~ . + logRS + logTarsus + Migration, test = "F")
m3 <- update(m2, formula = . ~ . + Migration)
add1(m3, scope = . ~ . + logRS + logTarsus, test = "F")
m4 <- update(m3, formula = . ~ . + logTarsus)
add1(m4, scope = . ~ . + logRS, test = "F")

# settle on m4

# backwards
drop1(m_full, test = "F")
m5 <- update(m_full, formula = . ~ . -logRS)
drop1(m5, test = "F")

# settle on m1

# AICc
m_full <- lm(data = d_new, relBeak ~ logRS + logTarsus + Migration + Trophic.Level + Primary.Lifestyle)

s <- MASS::stepAIC(m_full, scope = .~., direction = "both")


library(MuMIn)
library(AICcmodavg)
m_full <- lm(data = d_new, relBeak ~ logRS + Migration + Trophic.Level + logTarsus + Primary.Lifestyle, na.action = na.fail)
s <- dredge(m_full)
coef(s)
m.avg <- summary(model.avg(s, subset = delta < 4, fit = TRUE))
models <- list(m_full, m2, m3, m4, m5, m6, m7, m_null)
#specify model names
mod.names <- c("full", "m2", "m3", "m4", "m5", "m6",  "m7", "m_null")
aictab(models, mod.names)

library(naniar)
library(skimr)
library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ada-2022-datasets/main/Mammal_lifehistories_v2.txt"
d <- read_tsv(f, col_names = TRUE)
d <- d %>%
  replace_with_na_all(~.x == -999) %>%
  select(-c(refs, `litter size`))
d <- d %>% mutate(logMass = log(`mass(g)`),
                  logGestation = log(`gestation(mo)`),
                  logNewbornMass = log(`newborn(g)`),
                  logWeaning = log(`weaning(mo)`),
                  logWeaningMass = log(`wean mass(g)`),
                  logAFR = log(`AFR(mo)`),
                  logMaxLife = log(`max. life(mo)`),
                  logLitters = log(`litters/year`))

relGestation <- lm(logGestation ~ logMass, data = d, na.action=na.exclude)
relNewbornMass <- lm(logNewbornMass ~ logMass, data = d, na.action=na.exclude)
relWeaning <- lm(logWeaning ~ logMass, data = d, na.action=na.exclude)
relWeaningMass <- lm(logWeaningMass ~ logMass, data = d, na.action=na.exclude)
relAFR <- lm(logAFR ~ logMass, data = d, na.action=na.exclude)
relMaxLife <- lm(logMaxLife ~ logMass, data = d, na.action=na.exclude)

d <- d %>% mutate(
  relGestation = residuals(relGestation),
  relNewbornMass = residuals(relNewbornMass),
  relWeaning = residuals(relWeaning),
  relWeaningMass = residuals(relWeaningMass),
  relMaxLife = residuals(relMaxLife),
  relAFR = residuals(relAFR))

p <- ggplot(data = d, aes(x=order, y = relMaxLife)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

p <- ggplot(data = d, aes(x=order, y = relNewbornMass)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

p <- ggplot(data = d, aes(x=order, y = relWeaningMass)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

p

m_full <- lm(relMaxLife ~ relGestation + `litter size` + `litters/year` + order, data = d)
summary(m_full)


skim(d)
str(relMaxLife$residuals)
