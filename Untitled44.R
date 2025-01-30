library(tidyverse)

f <-
  "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/KamilarAndCooperData.csv"
d <-
  read_csv(f, col_names = TRUE) # creates a "tibble"

dim(d)
head(d)
tail(d)
str(d)
glimpse(d)
dim(d)
names(d) # or colnames(d)
rownames(d)

attach(d)
mean(Rest, na.rm = TRUE)
with(d, mean(Brain_Size_Female_Mean, na.rm = TRUE))
detach(d)
detach(package:kableExtra, unload = TRUE)
read_csv(f)
library(readr)
read_csv(f)
summary(d)

library(skimr)
skim(d)

attach(d)
boxplot(log(Body_mass_female_mean))
stripchart(log(d$Body_mass_female_mean),
             method = "jitter",
             col = "blue",
             vertical = TRUE,
             add = TRUE)

p <- ggplot(
  data = d,
  aes(x = "", y = log(Body_mass_female_mean))
  ) +
  geom_boxplot(na.rm = TRUE)

p <- ggplot(
  data = d,
  aes(x = Family, y = log(Body_mass_female_mean))
) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(color = "blue", width = 0.1)
p

p <- ggplot(data = d, aes(
  x = log(Body_mass_female_mean),
  y = log(Brain_Size_Female_Mean)
  )
) + geom_point()

