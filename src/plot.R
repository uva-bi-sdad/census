library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)


#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

apportionment <- read_csv("./output/apportionment.csv", col_names = TRUE)

apportionment <- gather(apportionment, "year", "seats", 2:11)
apportionment$year <- str_replace_all(apportionment$year, "seat", "")
apportionment$year <- as.integer(apportionment$year)


#
# Plot --------------------------------------------------------------------------------------------------
#

ggplot(apportionment, aes(x = year, y = seats, color = state)) +
  geom_line()