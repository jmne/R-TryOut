data <- read.csv("covid_19_daily_reports_10-27-2021.csv", header = TRUE, sep = " ")
gdata <- subset(data, Country_Region == "Germany")
print(gdata)
histogram <- hist(gdata$Deaths, freq = FALSE, prob = TRUE,
                  breaks = c(0, gdata$Deaths),
                  labels = gdata$Province_State,
                  xlab = "Bundesländer",
                  ylab = "emp. Dichte",
                  main = "Aufgabe 3 c)",
)