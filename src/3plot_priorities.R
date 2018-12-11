library(ggplot2)
library(dplyr)
library(openxlsx)
library(plotly)
library(tidyr)
library(stringr)
library(htmlwidgets)


#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

path <- "./output/priorityvalues.xlsx"

sheets <- openxlsx::getSheetNames(path)
sheetlist <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = path)
names(sheetlist) <- c("vals1990", "vals2000", "vals2010", "vals2011", "vals2012", "vals2013", "vals2014", "vals2015", "vals2016", "vals2017")

titles <- names(sheetlist)

for (i in seq_along(sheetlist)) {
  df <- gather(sheetlist[[i]], "step", "priorval", 2:386)
  df$step <- str_replace_all(df$step, "priority", "")
  df$step  <- as.integer(df$step)
  df$state <- str_to_lower(df$state)
  df$state <- str_replace_all(df$state, " ", "-")
  
  assign(titles[i], df)
}


#
# Plots for priority values by step: Dynamic plots ----------------------------------------------------------------------------------------------
#

pvplot1990 <- ggplot(vals1990, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 1990")
pvplot1990 <- ggplotly(pvplot1990)
pvplot1990
saveWidget(pvplot1990, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot1990.html"))

pvplot2000 <- ggplot(vals2000, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2000")
pvplot2000 <- ggplotly(pvplot2000)
pvplot2000
saveWidget(pvplot2000, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2000.html"))

pvplot2010 <- ggplot(vals2010, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2010")
pvplot2010 <- ggplotly(pvplot2010)
pvplot2010
saveWidget(pvplot2010, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2010.html"))

pvplot2011 <- ggplot(vals2011, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2011")
pvplot2011 <- ggplotly(pvplot2011)
pvplot2011
saveWidget(pvplot2011, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2011.html"))

pvplot2012 <- ggplot(vals2012, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2012")
pvplot2012 <- ggplotly(pvplot2012)
pvplot2012
saveWidget(pvplot2012, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2012.html"))

pvplot2013 <- ggplot(vals2013, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2013")
pvplot2013 <- ggplotly(pvplot2013)
pvplot2013
saveWidget(pvplot2013, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2013.html"))

pvplot2014 <- ggplot(vals2014, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2014")
pvplot2014 <- ggplotly(pvplot2014)
pvplot2014
saveWidget(pvplot2014, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2014.html"))

pvplot2015 <- ggplot(vals2015, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2015")
pvplot2015 <- ggplotly(pvplot2015)
pvplot2015
saveWidget(pvplot2015, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2015.html"))

pvplot2016 <- ggplot(vals2016, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2016")
pvplot2016 <- ggplotly(pvplot2016)
pvplot2016
saveWidget(pvplot2016, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2016.html"))

pvplot2017 <- ggplot(vals2017, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 16)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2017")
pvplot2017 <- ggplotly(pvplot2017)
pvplot2017
saveWidget(pvplot2017, file.path(getwd(), "output", "plots", "pvdynamic", "pvplot2017.html"))


#
# Plots for priority values by step: Static plots ----------------------------------------------------------------------------------------------
#

pvplot1990static <- ggplot(vals1990, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 1990") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot1990static.png", plot = pvplot1990static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2000static <- ggplot(vals2000, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2000") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2000static.png", plot = pvplot2000static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2010static <- ggplot(vals2010, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2010") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2010static.png", plot = pvplot2010static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2011static <- ggplot(vals2011, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2011") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2011static.png", plot = pvplot2011static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2012static <- ggplot(vals2012, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2012") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2012static.png", plot = pvplot2012static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2013static <- ggplot(vals2013, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2013") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2013static.png", plot = pvplot2013static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2014static <- ggplot(vals2014, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2014") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2014static.png", plot = pvplot2014static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2015static <- ggplot(vals2015, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2015") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2015static.png", plot = pvplot2015static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2016static <- ggplot(vals2016, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2016") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2016static.png", plot = pvplot2016static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")

pvplot2017static <- ggplot(vals2017, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 500, by = 50)) +
  scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
  expand_limits(y = c(1, 30000000)) +
  theme(axis.text = element_text(size = 7), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 2017") +
  facet_wrap(~state, nrow = 5)
ggsave("pvplot2017static.png", plot = pvplot2017static, device = png(), path = "./output/plots/pvstatic/", width = 26, height = 16, units = "cm")
