library(ggplot2)
library(dplyr)
library(openxlsx)
library(plotly)
library(tidyr)
library(stringr)

#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

path <- "./output/priorityvalues.xlsx"

sheets <- openxlsx::getSheetNames(path)
sheetlist <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = path)
names(sheetlist) <- c("vals1990", "vals2000", "vals2010", "vals2011", "vals2012", "vals2013", "vals2014", "vals2015", "vals2016", "vals2017")

list2env(sheetlist, .GlobalEnv)


#
# Plots for priority values 51-500 ------------------------------------------------------------------------------------------
#

pr1990 <- gather(vals1990, "step", "priorval", 2:386)
pr1990$step <- str_replace_all(pr1990$step, "priority", "")
pr1990$step  <- as.integer(pr1990$step)
pr1990$state <- str_to_lower(pr1990$state)
pr1990$state <- str_replace_all(pr1990$state, " ", "-")

valueplot <- ggplot(pr1990, aes(x = step, y = priorval, color = state)) +
  geom_line() +
  scale_x_continuous(breaks = seq(from = 1, to = 385, by = 50)) +
  scale_y_continuous(breaks = seq(from = 0, to = 21100000, by = 1000000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Step", y = "Priority value", color = "State", title = "State priority value by algorithm step, 1990")
valueplot <- ggplotly(valueplot)
valueplot
saveWidget(seatplot, file.path(getwd(), "output", "plots", "seatplot.html"))
