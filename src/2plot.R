library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(plotly)
library(treemapify) #install.packages("treemapify")
library(RColorBrewer)
library(htmlwidgets)


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
  scale_y_continuous(breaks = seq(from = 1, to = 60, by = 5)) +
  expand_limits(y = c(1, 56)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Year", y = "# Seats", color = "State", title = "Seats apportioned per state over time")
seatplot <- ggplotly(seatplot)
seatplot
saveWidget(seatplot, file.path(getwd(), "output", "plots", "seatplot.html"))


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
ggsave("chg9010p.png", plot = chg9010p, device = png(), path = "./output/plots/", width = 26, height = 16, units = "cm")

chg1117p <- ggplot(app1, aes(x = reorder(state, -pcgchg1117), y = pcgchg1117, color = pcgchg1117size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -35, to = 100, by = 5)) +
  expand_limits(y = c(-35, 100)) +
  labs(title = "Projected percent change in number of seats by state, 2011-2017", y = "Projected % change", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
ggsave("chg1117p.png", plot = chg1117p, device = png(), path = "./output/plots/", width = 26, height = 16, units = "cm")

chg9017p <- ggplot(app1, aes(x = reorder(state, -pcgchg9017), y = pcgchg9017, color = pcgchg9017size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -35, to = 100, by = 5)) +
  expand_limits(y = c(-35, 100)) +
  labs(title = "Projected percent change in number of seats by state, 1990-2017", y = "Projected % change", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
ggsave("chg9017p.png", plot = chg9017p, device = png(), path = "./output/plots/", width = 26, height = 16, units = "cm")


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
  labs(title = "Absolute change in number of seats by state, 1990 - 2010", y = "Change in seat number", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
ggsave("chg9010a.png", plot = chg9010a, device = png(), path = "./output/plots/", width = 26, height = 16, units = "cm")

chg1117a <- ggplot(app1, aes(x = reorder(state, -abschg1117), y = abschg1117, color = abschg1117size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -5, to = 9, by = 1)) +
  expand_limits(y = c(-5, 9)) +
  labs(title = "Projected absolute change in number of seats by state, 2011-2017", y = "Projected change in seat number", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
ggsave("chg1117a.png", plot = chg1117a, device = png(), path = "./output/plots/", width = 26, height = 16, units = "cm")

chg9017a <- ggplot(app1, aes(x = reorder(state, -abschg9017), y = abschg9017, color = abschg9017size)) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = -5, to = 9, by = 1)) +
  expand_limits(y = c(-5, 9)) +
  labs(title = "Projected absolute change in number of seats by state, 1990-2017", y = "Projected change in seat number", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95), legend.position = "none") +
  scale_color_manual(values = c("positive" = "#FFC107", "none" = "black", "negative" = "#1E88E5"))
ggsave("chg9017a.png", plot = chg9017a, device = png(), path = "./output/plots/", width = 26, height = 16, units = "cm")


#
# Treemaps -----------------------------------------------------------------------------------------------
#

seat1990plot <- ggplot(app1, aes(area = seat1990, fill = seat1990, label = paste(state, "(", seat1990, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 1990")
ggsave("tree1990.png", plot = seat1990plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2000plot <- ggplot(app1, aes(area = seat2000, fill = seat2000, label = paste(state, "(", seat2000, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2000")
ggsave("tree2000.png", plot = seat2000plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2010plot <- ggplot(app1, aes(area = seat2010, fill = seat2010, label = paste(state, "(", seat2010, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2010")
ggsave("tree2010.png", plot = seat2010plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2011plot <- ggplot(app1, aes(area = seat2011, fill = seat2011, label = paste(state, "(", seat2011, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2011")
ggsave("tree2011.png", plot = seat2011plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2012plot <- ggplot(app1, aes(area = seat2012, fill = seat2012, label = paste(state, "(", seat2012, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2012")
ggsave("tree2012.png", plot = seat2012plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2013plot <- ggplot(app1, aes(area = seat2013, fill = seat2013, label = paste(state, "(", seat2013, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2013")
ggsave("tree2013.png", plot = seat2013plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2014plot <- ggplot(app1, aes(area = seat2014, fill = seat2014, label = paste(state, "(", seat2014, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2014")
ggsave("tree2014.png", plot = seat2014plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2015plot <- ggplot(app1, aes(area = seat2015, fill = seat2015, label = paste(state, "(", seat2015, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2015")
ggsave("tree2015.png", plot = seat2015plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2016plot <- ggplot(app1, aes(area = seat2016, fill = seat2016, label = paste(state, "(", seat2016, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2016")
ggsave("tree2016.png", plot = seat2016plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

seat2017plot <- ggplot(app1, aes(area = seat2017, fill = seat2017, label = paste(state, "(", seat2017, ")", sep = ""))) +
  geom_treemap(layout = "squarified", show.legend = FALSE) +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Apportioned seats by state, 2017")
ggsave("tree2017.png", plot = seat2017plot, device = png(), path = "./output/plots/", width = 16, height = 16, units = "cm")

