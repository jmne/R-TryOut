cov_data <- read.csv2("a9/covid_19_daily_reports_12-12-2021.csv", header = TRUE)
world_data <- read.csv2("a9/World_Countries.csv", header = TRUE)
de_data <- read.csv2("a9/DE_Bundeslaender.csv", header = TRUE)
usa_data <- read.csv2("a9/USA_States.csv", header = TRUE)

cov_data_usa <- subset(cov_data, Country_Region == "US")

cov_data_agg_usa <- aggregate(cov_data_usa[c("Deaths", "Confirmed")], by = cov_data_usa["Province_State"], sum)

cov_data_agg <- aggregate(cov_data[c("Deaths", "Confirmed")], by = cov_data["Country_Region"], sum)
names(cov_data_agg)[3] <- "Confirmed"
names(cov_data_agg)[2] <- "Deaths"

names(usa_data)[1] <- "Province_State"
usa_data <- merge(cov_data_agg_usa, usa_data, by = "Province_State")


# Berechne relative Zahlen Deutschland
names(de_data) <- c("Bundesland", "Population")
de_data_merged <- merge(subset(cov_data, cov_data["Country_Region"] == "Germany"), de_data, by.y = "Bundesland", by.x = "Province_State")
de_data_merged["Relative_Deaths"] <- de_data_merged$Deaths / de_data_merged$Population
de_data_merged["Relative_Confirmed"] <- de_data_merged$Confirmed / de_data_merged$Population

usa_data["Relative_Deaths"] <- usa_data$Deaths / usa_data$Population
usa_data["Relative_Confirmed"] <- usa_data$Confirmed / usa_data$Population

world_data_merged <- merge(world_data, cov_data_agg, by.x = "Country", by.y = "Country_Region")
world_data_merged["Relative_Deaths"] <- round(world_data_merged$Deaths / world_data_merged$Population, 4)
world_data_merged["Relative_Confirmed"] <- round(world_data_merged$Confirmed / world_data_merged$Population, 4)

relatives <- c(sum(de_data_merged["Relative_Deaths"]), sum(de_data_merged["Relative_Confirmed"]),
               sum(usa_data["Relative_Deaths"]), sum(usa_data["Relative_Confirmed"]),
               sum(world_data_merged["Relative_Deaths"]), sum(world_data_merged["Relative_Confirmed"]))
names(relatives)[1:6] <- c("DE_D", "DE_C", "USA_D", "USA_C", "WOR_D", "WOR_C")

merged_relatives <- c(de_data_merged["Relative_Confirmed"], usa_data["Relative_Confirmed"], world_data_merged["Relative_Confirmed"])
names(merged_relatives)[1:3] <- c("DE", "USA", "WORLD")

boxplot(merged_relatives, range = 1, col = c("red", "green", "blue"))
title("Relative confirmed COVID Cases")

library(ineq)

plot(Lc(as.numeric(unlist(de_data_merged["Relative_Deaths"]))), col = "gold", main = "Lorenzkurve - Relative Todesfaelle Deutschland")
lines(Lc(as.numeric(unlist(usa_data["Relative_Deaths"]))), col = "darkblue")
lines(Lc(as.numeric(unlist(world_data_merged["Relative_Deaths"]))), col = "darkred")
legend("topleft", legend = c("Deutschland", "USA", "Welt"), fill = c("gold", "darkblue", "darkred"))