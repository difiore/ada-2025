# base R
f <- "~/Downloads/CPDS-1960-2014-reduced.csv"
d1 <- read.csv(f, header = TRUE)
glimpse(d1)
is.factor(d1$country)

f <- "~/Downloads/CPDS-1960-2014-reduced.txt"
d2 <- read.delim(f, header = TRUE, sep = "\t")
glimpse(d1)

# tidyverse/readr
library(tidyverse)
f <- "~/Downloads/CPDS-1960-2014-reduced.csv"
d3 <- read_csv(f, col_names = TRUE)
glimpse(d3)

f <- "~/Downloads/CPDS-1960-2014-reduced.txt"
d4 <-read_delim(f, col_names = TRUE, delim = "\t")
glimpse(d4)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/CPDS-1960-2014-reduced.csv"
d5 <- read_csv(f, col_names = TRUE)
glimpse(d5)

# readxl
library(readxl)
f <- "~/Downloads/CPDS-1960-2014-reduced.xlsx"
d6 <-read_excel(f, sheet = 1, col_names = TRUE)
glimpse(d6)

# decimal points get truncated when saving from excel to csv!

library(rdrop2)
drop_auth(new_user = TRUE)  # opens a browser dialog box to ask for authorization...
drop_dir()  # lists the contents of your dropbox folder
f <- "CPDS-1960-2014-reduced.csv"  # name of the file to read from
f <- drop_search(query = f, mode = "filename")
# searches your dropbox directory for file or directory names... this can be
# slow!
filenames <- vector()
for (i in 1:length(f$matches)) {
  filenames <- c(filenames, f$matches[[i]]$metadata$path_display)
  # this is the location of the results returned above the [[i]] returns each
  # encountered file with a matching filename and puts them into a vector
}
d <- drop_read_csv(filenames[1], header = TRUE, sep = ",", stringsAsFactors = FALSE)
# here the [1] reads only the first file from filenames, but this can be
# modified this to read more than one file
detach(package:rdrop2)
