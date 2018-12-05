library(readr)
library(congappor) # devtools::install_github("coletl/congappor")
library(dplyr)
library(stringr)
#library(xlsx)
library(openxlsx)

# Notes: Two separate files because:
#          Simulations and input for 1990-2010 are based on/match the 1990-2010 Apportionment2.xlsx file (onedrive, census, data).
#          Simulations and input for 2011-2017 are based on the Population2010_2017.xlsx file (onedrive, census, data).
#        Repetitive code because:
#          apportion won't work in a loop. Working to see if I can fix this.
  

#
# Prepare data --------------------------------------------------------------------------------------------------------------
#

pop_1990_2010 <- read_csv("./output/pop1990_2010.csv", col_names = TRUE)
pop_2011_2017 <- read_csv("./output/pop2011_2017.csv", col_names = TRUE)

# Make input dataframes by year
popcols1 <- c("p1990", "p2000", "p2010")
for (i in popcols1) {
  df <- pop_1990_2010 %>% select(state, i) %>% rename(pop = i)
  assign(paste("c", i, sep = ""), df)
}

popcols2 <- c("p2011", "p2012", "p2013", "p2014", "p2015", "p2016", "p2017")
for (i in popcols2) {
  df <- pop_2011_2017 %>% select(state, i) %>% rename(pop = i)
  assign(paste("c", i, sep = ""), df)
}


#
# Replicate 1990 - 2010 -----------------------------------------------------------------------------------------------------
#

# 1990
sim1990 <- apportion(cp1990, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score1990 <- priority_scores
order1990 <- seat_order

# 2000
sim2000 <- apportion(cp2000, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2000 <- priority_scores
order2000 <- seat_order

# 2010
sim2010 <- apportion(cp2010, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2010 <- priority_scores
order2010 <- seat_order


#
# Project 2011-2017 ---------------------------------------------------------------------------------------------------------
#

# 2011
sim2011 <- apportion(cp2011, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2011 <- priority_scores
order2011 <- seat_order

# 2012
sim2012 <- apportion(cp2012, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2012 <- priority_scores
order2012 <- seat_order

# 2013
sim2013 <- apportion(cp2013, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2013 <- priority_scores
order2013 <- seat_order

# 2014
sim2014 <- apportion(cp2014, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2014 <- priority_scores
order2014 <- seat_order

# 2015
sim2015 <- apportion(cp2015, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2015 <- priority_scores
order2015 <- seat_order

# 2016
sim2016 <- apportion(cp2016, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2016 <- priority_scores
order2016 <- seat_order

# 2017
sim2017 <- apportion(cp2017, total_seats = 435, DC_seats = FALSE, PR_seats = FALSE, GU_seats = FALSE, 
                     min_seats = 1, store_priority = TRUE, store_seat_order = TRUE)
score2017 <- priority_scores
order2017 <- seat_order


#
# Create result dataframes ---------------------------------------------------------------------------------------------------------
#

# Seats
apport <- data.frame(state = sim1990$state, seat1990 = sim1990$seats, seat2000 = sim2000$seats, seat2010 = sim2010$seats, 
                        seat2011 = sim2011$seats, seat2012 = sim2012$seats, seat2013 = sim2013$seats, seat2014 = sim2014$seats, 
                        seat2015 = sim2015$seats, seat2016 = sim2016$seats, seat2017 = sim2017$seats)
apport$state <- str_to_lower(apport$state)
apport$state <- str_replace_all(apport$state, " ", "-")

# Order
ord <- data.frame(step = c(1:385), order1990 = order1990, order2000 = order2000, order2010 = order2010, order2011 = order2011,
                     order2012 = order2012, order2013 = order2013, order2014 = order2014, order2015 = order2015,
                     order2016 = order2016, order2017 = order2017)
ord <- ord %>% mutate_all(funs(str_replace_all(., " ", "-")))
ord <- data.frame(lapply(ord, tolower))

# Scores
scr <- list("1990" = score1990, "2000" = score2000, "2010" = score2010, "2011" = score2011, "2012" = score2012, "2013" = score2013, 
            "2014" = score2014, "2015" = score2015, "2016" = score2016, "2017" = score2017)


#
# Write out ---------------------------------------------------------------------------------------------------------------------------
#

# Apportionment (seats) and seat assignment order
write_csv(apport, "./output/apportionment.csv", append = FALSE, col_names = TRUE)
write_csv(ord, "./output/seatorder.csv", append = FALSE, col_names = TRUE)

# Population estimates
pop_total <- merge(pop_1990_2010, pop_2011_2017, by = "state")
write_csv(pop_total, "./output/population.csv", append = FALSE, col_names = TRUE)

# Priority values
# for (i in c(1:10)){
#   write.xlsx(scr[i], file = "./output/priorityvals.xlsx", sheetName = paste(i), append = T)
# }

write.xlsx(scr, file = "./output/priorityvals.xlsx")

#
# Clean up working environment ---------------------------------------------------------------------------------------------------------
#

remove(list = c(ls(pattern = "pop_"), ls(pattern = "df"), ls(pattern = "popcols"), ls(pattern = "seat_order"), 
                ls(pattern = "priority_scores"), ls(pattern = "cp"), ls(pattern = "cp"), ls(pattern = "score"), 
                ls(pattern = "order"), ls(pattern = "sim"), ls(pattern = "i")))







