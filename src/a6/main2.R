library("dplyr")
library("maps")
data <- read.csv("ü6/time_series_covid19_confirmed_11-21-2021.csv", header = TRUE, sep = " ")
gdata <- subset(subset(data, Country_Region == 'Germany'), select = 306:325)
growth_rate <- function(x)(x / lag(x) - 1) * 100
gdata$growth_rate <- growth_rate(gdata %>% select_if(is.numeric))
print(gdata)
map("world", fill = TRUE, col = "white", bg = "lightblue",
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))