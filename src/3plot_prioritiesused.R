library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)


#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

path <- "./output/priorityvalues.xlsx"

sheets <- openxlsx::getSheetNames(path)
sheetlist <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = path)
names(sheetlist) <- c("vals1990", "vals2000", "vals2010", "vals2011", "vals2012", "vals2013", "vals2014",
                      "vals2015", "vals2016", "vals2017")

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
# Extract and rank unique priority values ----------------------------------------------------------------
#

dflist <- list(vals1990, vals2000, vals2010, vals2011, vals2012, vals2013, vals2014, vals2015, vals2016, vals2017)

for (i in seq_along(dflist)) {
  df <- dflist[[i]] %>%
    group_by(state) %>%
    distinct(priorval) %>%
    arrange(desc(priorval), .by_group = TRUE) %>%
    mutate(seq = row_number())
  
  assign(titles[i], df)
}


#
# Plot --------------------------------------------------------------------------------------------------
#

plotvals <- function(df, year) {
  ggplot(df, aes(x = seq, y = priorval, fill = state)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_x_continuous(breaks = seq(from = 1, to = 55, by = 5)) +
    scale_y_continuous(breaks = seq(from = 1, to = 30000000, by = 5000000)) +
    expand_limits(y = c(1, 30000000)) +
    theme(axis.text = element_text(size = 6), axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1), legend.position = "none") +
    labs(x = "Value # in sequence", y = "Priority value", color = "State", title = paste("Priority values used by state, ", year, sep = "")) +
    facet_wrap(~state, nrow = 5)
  
  ggsave(paste("usedvals", year, ".png", sep = ""), device = png(), path = "./output/plots/usedvals/", width = 26, height = 16, units = "cm")
}

plotvals(vals1990, "1990")
plotvals(vals2000, "2000")
plotvals(vals2010, "2010")
plotvals(vals2011, "2011")
plotvals(vals2012, "2012")
plotvals(vals2013, "2013")
plotvals(vals2014, "2014")
plotvals(vals2015, "2015")
plotvals(vals2016, "2016")
plotvals(vals2017, "2017")



