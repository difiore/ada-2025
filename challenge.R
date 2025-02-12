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

sammy <- d |>
  filter(Focal.Animal == "Sammy")
p <- ggplot(sammy, aes(x = easting, y = northing)) +
  geom_point()
p

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

