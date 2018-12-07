library(readr)
library(congappor) # devtools::install_github("coletl/congappor")
library(dplyr)
library(stringr)
library(openxlsx)

# Notes: 
# Simulations and input for 1990-2010 are based on/match the 1990-2010 Apportionment2.xlsx file (onedrive, census, data).
# Simulations and input for 2011-2017 are based on the Population2010_2017.xlsx file (onedrive, census, data).

# VERSION WITH HYPOTHETICAL 500 SEATS TO SEE EXTRA PRIORITY VALUES.

#
# Prepare data --------------------------------------------------------------------------------------------------------------
#

population <- read_csv("./output/population.csv", col_names = TRUE)

# Make input dataframes by year
popcols <- c("p1990", "p2000", "p2010", "p2011", "p2012", "p2013", "p2014", "p2015", "p2016", "p2017")
for (i in popcols) {
  df <- population %>% select(state, i) %>% rename(pop = i)
  assign(paste("c", i, sep = ""), df)
}


#
# Project 500 seats for 1990-2010, 2011-2017 ------------------------------------------------------------------------------------
#

dflist <- list("1990" = cp1990, "2000" = cp2000, "2010" = cp2010,
               "2011" = cp2011, "2012" = cp2012, "2013" = cp2013, "2014" = cp2014, "2015" = cp2015, "2016" = cp2016, "2017" = cp2017)
titles <- names(dflist)

for (i in seq_along(dflist)) {
  app <- apportion(dflist[[i]], total_seats = 500, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                   min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
  scores <- priority_scores
  order <- seat_order
  
  assign(paste("apportion5", titles[i], sep = ""), app)
  assign(paste("priorityvals5", titles[i], sep = ""), scores)
  assign(paste("seatorder5", titles[i], sep = ""), order)
}


#
# Create result dataframes ---------------------------------------------------------------------------------------------------------
#

# Seats
apportionment5 <- data.frame(state = apportion51990$state, seat1990 = apportion51990$seats, seat2000 = apportion52000$seats, seat2010 = apportion52010$seats, 
                            seat2011 = apportion52011$seats, seat2012 = apportion52012$seats, seat2013 = apportion52013$seats, seat2014 = apportion52014$seats, 
                            seat2015 = apportion52015$seats, seat2016 = apportion52016$seats, seat2017 = apportion52017$seats)
apportionment5$state <- str_to_lower(apportionment5$state)
apportionment5$state <- str_replace_all(apportionment5$state, " ", "-")

# Order
seatorder5 <- data.frame(step = c(1:450), order1990 = seatorder51990, order2000 = seatorder52000, order2010 = seatorder52010, order2011 = seatorder52011,
                        order2012 = seatorder52012, order2013 = seatorder52013, order2014 = seatorder52014, order2015 = seatorder52015,
                        order2016 = seatorder52016, order2017 = seatorder52017)
seatorder5 <- seatorder5 %>% mutate_all(funs(str_replace_all(., " ", "-")))
seatorder5 <- data.frame(lapply(seatorder5, tolower))

# Scores
priorityvalues5 <- list("1990" = priorityvals51990, "2000" = priorityvals52000, "2010" = priorityvals52010, "2011" = priorityvals52011, 
                       "2012" = priorityvals52012, "2013" = priorityvals52013, "2014" = priorityvals52014, "2015" = priorityvals52015, 
                       "2016" = priorityvals52016, "2017" = priorityvals52017)


#
# Write out ---------------------------------------------------------------------------------------------------------------------------
#

# Apportionment (seats) and seat assignment order
write_csv(apportionment5, "./output/apportionment5.csv", append = FALSE, col_names = TRUE)
write_csv(seatorder5, "./output/seatorder5.csv", append = FALSE, col_names = TRUE)


# Priority values
write.xlsx(priorityvalues5, file = "./output/priorityvalues5.xlsx")


#
# Clean up working environment ---------------------------------------------------------------------------------------------------------
#

rm(list = ls())