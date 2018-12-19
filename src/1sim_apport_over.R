library(readr)
library(congappor) # devtools::install_github("coletl/congappor")
library(dplyr)
library(stringr)
library(openxlsx)

# Notes: 
# Input for simulations with population counts that include a *constant 2010 overseas population count.*
# 2010 overseas count = 2010 apportionment minus 2010 census
# The 2010 overseas count is then added to population estimates from 2011-2017.
# Population estimates from 2011-2017 are taken from "Annual Estimates of the Resident Population 
# for the United States, Regions, States, and Puerto Rico: April 1, 2010 to July 1, 2017."
# https://www2.census.gov/programs-surveys/popest/tables/2010-2017/state/totals/nst-est2017-01.xlsx

#
# Prepare data --------------------------------------------------------------------------------------------------------------
#

population <- read_csv("./output/population_over.csv", col_names = TRUE)

# Make input dataframes by year
popcols <- c("p2010", "p2011", "p2012", "p2013", "p2014", "p2015", "p2016", "p2017")
for (i in popcols) {
  df <- population %>% select(state, i) %>% rename(pop = i)
  assign(paste("c", i, sep = ""), df)
}


#
# Project 2010-2017 ------------------------------------------------------------------------------------
#

dflist <- list("2010" = cp2010, "2011" = cp2011, "2012" = cp2012, "2013" = cp2013, "2014" = cp2014, 
               "2015" = cp2015, "2016" = cp2016, "2017" = cp2017)
titles <- names(dflist)

for (i in seq_along(dflist)) {
  app <- apportion(dflist[[i]], total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                   min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
  scores <- priority_scores
  order <- seat_order
  
  assign(paste("apportion", titles[i], sep = ""), app)
  assign(paste("priorityvals", titles[i], sep = ""), scores)
  assign(paste("seatorder", titles[i], sep = ""), order)
}


#
# Create result dataframes ---------------------------------------------------------------------------------------------------------
#

# Seats
apportionment <- data.frame(state = apportion2010$state, seat2010 = apportion2010$seats, seat2011 = apportion2011$seats, 
                            seat2012 = apportion2012$seats, seat2013 = apportion2013$seats, seat2014 = apportion2014$seats, 
                            seat2015 = apportion2015$seats, seat2016 = apportion2016$seats, seat2017 = apportion2017$seats)
apportionment$state <- str_to_lower(apportionment$state)
apportionment$state <- str_replace_all(apportionment$state, " ", "-")

# Order
seatorder <- data.frame(step = c(1:385), order2010 = seatorder2010, order2011 = seatorder2011, order2012 = seatorder2012, 
                        order2013 = seatorder2013, order2014 = seatorder2014, order2015 = seatorder2015,
                        order2016 = seatorder2016, order2017 = seatorder2017)
seatorder <- seatorder %>% mutate_all(funs(str_replace_all(., " ", "-")))
seatorder <- data.frame(lapply(seatorder, tolower))

# Scores
priorityvalues <- list("2010" = priorityvals2010, "2011" = priorityvals2011, "2012" = priorityvals2012, 
                       "2013" = priorityvals2013, "2014" = priorityvals2014, "2015" = priorityvals2015, 
                       "2016" = priorityvals2016, "2017" = priorityvals2017)


#
# Write out ---------------------------------------------------------------------------------------------------------------------------
#

# Apportionment (seats) and seat assignment order
write_csv(apportionment, "./output/apportionment_over.csv", append = FALSE, col_names = TRUE)
write_csv(seatorder, "./output/seatorder_over.csv", append = FALSE, col_names = TRUE)


# Priority values
write.xlsx(priorityvalues, file = "./output/priorityvalues_over.xlsx")


#
# Clean up working environment ---------------------------------------------------------------------------------------------------------
#

rm(list = ls())

