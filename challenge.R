














my_filter <- function(x, condition, variable){
  library(tidyverse)
  x <- x |> filter(!!sym(variable) %in% condition)
  return(x)
}

df <- data.frame(rowid = 1:5, value = c("a","b", "c", "d", "e"))

my_filter(df) # gives an error b/c function definition does not include a default value for the arguments

my_filter(df, condition = c("c", "e"), variable = "value")
my_filter(df, condition = c(1,2), variable = "rowid")

library(tidyverse)

gps <- read_csv("/Users/ad26693/Development/Repos/ada-datasets/sample_gps_data.csv", col_names = TRUE)
beh <- read_csv("/Users/ad26693/Development/Repos/ada-datasets/sample_behavioral_data.csv", col_names = TRUE)
beh <- beh |>
  rowwise() |>
  mutate(year = str_split(`Date.Time`, "-")[[1]][1]) |>
  filter(year %in% c(2012, 2013, 2014))

d <-
  left_join(beh, gps, by = c("Date.Time" = "Date.Time", "Observer" = "Observer")) |>
  filter(!is.na(`Mean.Longitude`), !is.na(`Mean.Latitude`))

library(oce)
d <- d |>
  rowwise() |>
  mutate(easting = lonlat2utm(`Mean.Longitude`,`Mean.Latitude`)$easting,
         northing = lonlat2utm(`Mean.Longitude`, `Mean.Latitude`)$northing + 10000000)

ana <- d |>
  filter(Focal.Animal == "Ana")
p <- ggplot(ana, aes(x = easting, y = northing)) +
  geom_point()
p




x <- c(5, 6, 7, 4, 6, 7, 7, 6)


my_pop_var <- function(x){
  v <- sum((x - mean(x))^2)/length(x)
  return(v)
}

my_sample_var <- function(x){
  v <- sum((x - mean(x))^2)/(length(x) - 1)
  return(v)
}

(a <- my_pop_var(x))
(b <- my_sample_var(x))
(c <- var(x))



sammy <- d |>
  filter(str_detect(`Group.Comp`, "Sammy"))
p <- ggplot(sammy, aes(x = easting, y = northing)) +
  geom_point()
p

d <- d |>
  separate_wider_delim(Group.Comp, " ", names_sep="-", too_few = "align_start")

sammy <- d |>
  filter(if_any(matches("Group"), ~ str_detect(., "Sammy")))
p <- ggplot(sammy, aes(x = easting, y = northing)) +
  geom_point()
p


library(mosaic)
mu <-10
sigma <- 2
plotDist("norm", mean=mu, sd=sigma, xlab="x", ylab="Frequency")

s1 <- rnorm(n = 10, mean = 10, sd = 2)
histogram(s1)
mean(s1)
sd(s1)

s2 <- rnorm(n = 1000, mean = 10, sd = 2)
histogram(s2)
mean(s2)
sd(s2)

s3 <- rnorm(n = 1000, mean = 15, sd = 5)
histogram(s3)
mean(s3)
sd(s3)


histogram(s2 + s3)



