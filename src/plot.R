library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(plotly)
library(treemap) #install.packages("treemap")
library(RColorBrewer)
library(htmlwidgets)


# Note: Can't save widgets in directories (known issue with Plotly). Have to manually move saved files after. 


#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

app1 <- read_csv("./output/apportionment.csv", col_names = TRUE)

app2 <- gather(app1, "year", "seats", 2:11)
app2$year <- str_replace_all(app2$year, "seat", "")
app2$year <- as.integer(app2$year)


#
# Apportioned seats over time -----------------------------------------------------------------------------------------------
#

seatplot <- ggplot(app2, aes(x = year, y = seats, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = c(1990, 2000, seq(from = 2010, to = 2017, by = 1))) +
  scale_y_continuous(breaks = seq(from = 1, to = 55, by = 5)) +
  expand_limits(y = c(1, 55)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Year", y = "# Seats", color = "State", title = "Seats apportioned per state over time")
seatplot <- ggplotly(seatplot)
seatplot
saveWidget(seatplot, file = "seatplot.html")

#
# Percent change over time -----------------------------------------------------------------------------------------------
#

app1 <- app1 %>% 
  mutate(pcgchg9010 = ((seat2010 - seat1990)/seat1990)*100,
         pcgchg1117 = ((seat2017 - seat2011)/seat2011)*100,
         pcgchg9017 = ((seat2017 - seat1990)/seat1990)*100,
         pcgchg9010size = as.factor(ifelse(pcgchg9010 > 0, "positive",
                                           ifelse(pcgchg9010 < 0, "negative",
                                                  ifelse(pcgchg9010 == 0, "none", "NA")))),
         pcgchg1117size = as.factor(ifelse(pcgchg1117 > 0, "positive",
                                           ifelse(pcgchg1117 < 0, "negative",
                                                  ifelse(pcgchg1117 == 0, "none", "NA")))),
         pcgchg9017size = as.factor(ifelse(pcgchg9017 > 0, "positive",
                                           ifelse(pcgchg9017 < 0, "negative",
                                                  ifelse(pcgchg9017 == 0, "none", "NA")))))
app1$state <- as.factor(app1$state)

chg9010p <- ggplot(app1, aes(x = reorder(state, -pcgchg9010), y = pcgchg9010, color = pcgchg9010size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -35, to = 100, by = 5)) +
  expand_limits(y = c(-35, 100)) +
  labs(title = "Percent change in number of seats by state, 1990 - 2010", y = "% change", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
chg9010p <- ggplotly(chg9010p)
chg9010p
saveWidget(chg9010p, file = "chg9010p.html")

chg1117p <- ggplot(app1, aes(x = reorder(state, -pcgchg1117), y = pcgchg1117, color = pcgchg1117size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -35, to = 100, by = 5)) +
  expand_limits(y = c(-35, 100)) +
  labs(title = "Projected percent change in number of seats by state, 2011-2017", y = "Projected % change", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
chg1117p <- ggplotly(chg1117p)
chg1117p
saveWidget(chg1117p, file = "chg1117p.html")

chg9017p <- ggplot(app1, aes(x = reorder(state, -pcgchg9017), y = pcgchg9017, color = pcgchg9017size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -35, to = 100, by = 5)) +
  expand_limits(y = c(-35, 100)) +
  labs(title = "Projected percent change in number of seats by state, 1990-2017", y = "Projected % change", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
chg9017p <- ggplotly(chg9017p)
chg9017p
saveWidget(chg9017p, file = "chg9017p.html")

#
# Absolute change over time -----------------------------------------------------------------------------------------------
#

app1 <- app1 %>% 
  mutate(abschg9010 = (seat2010 - seat1990),
         abschg1117 = (seat2017 - seat2011),
         abschg9017 = (seat2017 - seat1990),
         abschg9010size = as.factor(ifelse(abschg9010 > 0, "positive",
                                           ifelse(abschg9010 < 0, "negative",
                                                  ifelse(abschg9010 == 0, "none", "NA")))),
         abschg1117size = as.factor(ifelse(abschg1117 > 0, "positive",
                                           ifelse(abschg1117 < 0, "negative",
                                                  ifelse(abschg1117 == 0, "none", "NA")))),
         abschg9017size = as.factor(ifelse(abschg9017 > 0, "positive",
                                           ifelse(abschg9017 < 0, "negative",
                                                  ifelse(abschg9017 == 0, "none", "NA")))))

chg9010a <- ggplot(app1, aes(x = reorder(state, -abschg9010), y = abschg9010, color = abschg9010size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -5, to = 9, by = 1)) +
  expand_limits(y = c(-5, 9)) +
  labs(title = "Projected absolute change in number of seats by state, 1990 - 2010", y = "Change in seat number", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
chg9010a <- ggplotly(chg9010a)
chg9010a
saveWidget(chg9010a, file = "chg9010a.html")

chg1117a <- ggplot(app1, aes(x = reorder(state, -abschg1117), y = abschg1117, color = abschg1117size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -5, to = 9, by = 1)) +
  expand_limits(y = c(-5, 9)) +
  labs(title = "Projected absolute change in number of seats by state, 2011-2017", y = "Projected change in seat number", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
chg1117a <- ggplotly(chg1117a)
chg1117a
saveWidget(chg1117a, file = "chg1117a.html")

chg9017a <- ggplot(app1, aes(x = reorder(state, -abschg9017), y = abschg9017, color = abschg9017size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -5, to = 9, by = 1)) +
  expand_limits(y = c(-5, 9)) +
  labs(title = "Projected absolute change in number of seats by state, 1990-2017", y = "Projected change in seat number", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
chg9017a <- ggplotly(chg9017a)
chg9017a
saveWidget(chg9017a, file = "chg9017a.html")


#
# Treemaps -----------------------------------------------------------------------------------------------
#

tree1990 <- treemap(app1, index = "state", vSize = "seat1990", title = "Apportioned seats by state, 1990", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2000 <- treemap(app1, index = "state", vSize = "seat2000", title = "Apportioned seats by state, 2000", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2010 <- treemap(app1, index = "state", vSize = "seat2010", title = "Apportioned seats by state, 2010", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2011 <- treemap(app1, index = "state", vSize = "seat2011", title = "Apportioned seats by state, 2011", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2012 <- treemap(app1, index = "state", vSize = "seat2012", title = "Apportioned seats by state, 2012", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2013 <- treemap(app1, index = "state", vSize = "seat2013", title = "Apportioned seats by state, 2013", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2014 <- treemap(app1, index = "state", vSize = "seat2014", title = "Apportioned seats by state, 2014", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2015 <- treemap(app1, index = "state", vSize = "seat2015", title = "Apportioned seats by state, 2015", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2016 <- treemap(app1, index = "state", vSize = "seat2016", title = "Apportioned seats by state, 2016", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")
tree2017 <- treemap(app1, index = "state", vSize = "seat2017", title = "Apportioned seats by state, 2017", 
                    palette = "Paired", border.col = "white", fontcolor.labels = "black")



