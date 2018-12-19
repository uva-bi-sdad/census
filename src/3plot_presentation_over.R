library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)


#
# Read in and prepare data --------------------------------------------------------------------------------------------------
#

app1 <- read_csv("./output/apportionment_over.csv", col_names = TRUE)

app2 <- gather(app1, "year", "seats", 2:8)
app2$year <- str_replace_all(app2$year, "seat", "")
app2$year <- as.integer(app2$year)


app1 <- app1 %>% 
  mutate(abschg1017 = (seat2017 - seat2010),
         abschg1017size = as.factor(ifelse(abschg1017 > 0, "positive",
                                           ifelse(abschg1017 < 0, "negative",
                                                  ifelse(abschg1017 == 0, "none", "NA")))))

chg1017a <- ggplot(app1, aes(x = reorder(state, -abschg1017), y = abschg1017, color = abschg1017size)) +
  geom_point(size = 7) +
  scale_y_continuous(breaks = seq(from = -1, to = 2, by = 1)) +
  expand_limits(y = c(-1, 2)) +
  labs(title = "Projected absolute change in number of seats by state, 2010-2017", y = "Projected change in seat number", x = "",
       caption = "Note: 2010 apportionment based on 2010 apportionment population. \n Apportionment for 2011-2017 based on population estimate adjusted for 2010 overseas population.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95, size = 13), legend.position = "none") +
  scale_color_manual(values = c("positive" = "orange", "none" = "black", "negative" = "#1E88E5")) 
ggsave("presentationplot1_over.png", plot = chg1017a, device = png(), path = "./output/plots/", width = 35, height = 16, units = "cm")







