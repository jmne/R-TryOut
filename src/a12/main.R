data <- read.csv2("a12/covid_19_daily_reports_01-23-2022.csv", header = TRUE)

data_agg <- aggregate(data[c("Deaths", "Confirmed")], by = data["Country_Region"], sum)

slclust <- hclust(dist(data_agg$Confirmed), method = "single")
coclust <- hclust(dist(data_agg$Confirmed), method = "complete")
avclust <- hclust(dist(data_agg$Confirmed), method = "average")
waclust <- hclust(dist(data_agg$Confirmed), method = "ward.D2")


plot(slclust, main = "Dendogram", sub = "Single Link Clustering")
plot(coclust, main = "Dendogram", sub = "Complete Link Clustering")
plot(avclust, main = "Dendogram", sub = "Average Link Clustering")
plot(waclust, main = "Dendogram", sub = "Ward Clustering")
