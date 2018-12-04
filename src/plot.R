library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(plotly)

#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

app1 <- read_csv("./output/apportionment.csv", col_names = TRUE)

app2 <- gather(app1, "year", "seats", 2:11)
app2$year <- str_replace_all(app2$year, "seat", "")
app2$year <- as.integer(app2t$year)


#
# Apportioned seats over time -----------------------------------------------------------------------------------------------
#

breaks <- c(1990, 2000, seq(from = 2010, to = 2017, by = 1))
           
seatplot <- ggplot(app2, aes(x = year, y = seats, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = c(1990, 2000, seq(from = 2010, to = 2017, by = 1))) +
  scale_y_continuous(breaks = seq(from = 1, to = 55, by = 5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Year", y = "# Seats", color = "State", title = "Seats apportioned per state over time")

seatplot <- ggplotly(seatplot)
seatplot


#
# Percent change over time -----------------------------------------------------------------------------------------------
#

app1 <- app1 %>% 
          mutate(pcgchg9010 = ((seat2010 - seat1990)/seat1990)*100,
                 pcgchg1117 = ((seat2017 - seat2011)/seat2011)*100)

chg9010 <- ggplot(app1, aes(x = state, y = pcgchg9010)) +
  geom_point() +
  labs(title = "Percent change in number of seats by state, 1990 - 2010", y = "% change") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95))
chg9010

chg1117 <- ggplot(app1, aes(x = state, y = pcgchg1117)) +
  geom_point() +
  labs(title = "Projected percent change in number of seats by state, 2011-2017", y = "Projected % change") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95))
chg1117







