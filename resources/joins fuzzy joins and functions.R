library(tidyverse)
p <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/papers.csv", col_names = TRUE)
c <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/creators.csv", col_names = TRUE)

head(p)
head(c)

p <- p |>
  separate_wider_delim(cols = Author,
                       delim = ";",
                       names = c("First Author", "A2", "A3", "A4"),
                       too_few = "align_start", too_many = "drop") |>
  mutate(A2 = str_trim(`A2`, "both"),
         A3 = str_trim(`A3`, "both"),
         A4 = str_trim(`A4`, "both"))

head(p)

c <- c |>
  distinct()
head(c)

inner <- inner_join(c, p, by = c("fullName" = "First Author"))
inner <- inner_join(p, c, by = c("First Author" = "fullName"))
left <- left_join(c, p, by = c("fullName" = "First Author"))
right <- right_join(p, c, by = c("First Author" = "fullName"))
find_pubs <- tibble(fullName = c("Abbott, David H"))
inner <- inner_join(find_pubs, p, by = c("fullName" = "First Author"))

library(fuzzyjoin)
find_pubs <- tibble(partialName = c("^Abbott"))
inner_fuzzy <- regex_inner_join(p, find_pubs, by = c("First Author" = "partialName"), ignore_case = FALSE)

find_pubs <- tibble(partialName = c("^Wil", "ony$"))
inner_fuzzy <- regex_inner_join(p, find_pubs, by = c("First Author" = "partialName"), ignore_case = FALSE)

my_filter <- function(x, condition, variable){
  library(tidyverse)
  x <- x |> filter(!!sym(variable) %in% condition)
  return(x)
}

df <- data.frame(rowid = 1:5, value = c("a","b", "c", "d", "e"))
my_filter(df, condition = c("c", "e"), variable = "value")
my_filter(df, condition = c("a", "b", "e"), variable = "value")

library(tidyverse)

gps <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/sample_gps_data.csv", col_names = TRUE)
beh <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/sample_spider_monkey_data.csv", col_names = TRUE)
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
