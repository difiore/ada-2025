# ANOVA
# libraries
library(tidyverse)
library(car)
library(jtools)
library(infer)

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
