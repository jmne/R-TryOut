data <- read.table("ü7/covid_19_daily_reports_11-28-2021.csv", header = TRUE, sep = ";")
#data$Province_State <- NULL
sumed_data <- aggregate(x = data[colnames(data) != "Country_Region" & colnames(data) != "Province_State"], by = list(data$Country_Region), FUN = sum)
colnames(sumed_data) <- c("Country_Region", "Confirmed", "Deaths")
str(sumed_data)
sumed_confirmed <- sum(sumed_data$Confirmed)

#Median berechnen
sumed_data_conf_med <- sort(x = sumed_data$Confirmed)

n <- length(sumed_data_conf_med)

if (n %% 2 == 0) {
  medconf <- round(sumed_data_conf_med[floor(n / 2)] + sumed_data_conf_med[ceiling(n / 2)]) / 2
}else {
  medconf <- sumed_data_conf_med[ceiling(n / 2)]
}

func_m <- function(x) { abs(sumed_data_conf_med[x] - medconf) }
absmed_data <- 1 / n * integrate(func_m, lower = 1, upper = n, subdivisions = 1000)$value
print(absmed_data)

#empirische Varianz

func_v <- function(x) { (sumed_data_conf_med[x] - (sum(sumed_data_conf_med) / n))^2 }

varianz <- 1 / (n - 1) * integrate(func_v, lower = 1, upper = n, subdivisions = 1000)$value
print(varianz)

#empir. Standarabweichung

print(sqrt(varianz))


#======= c =========

#Mittelwert
print("Mittelwert:")
print(sum(sumed_data_conf_med) / n)

#Median
print("Median:")
print(medconf)

drawing <- c(sum(sumed_data_conf_med) / n, medconf)

pie(drawing, labels = c("Mittelwert", "Median"))

hist(sumed_data_conf_med, main = "Histogram", col = "steelblue", xlab = "daten", ylab = "Aufkommen", breaks = seq(0, max(sumed_data_conf_med), length = 10000))
box()
library("moments")
skewness(sumed_data_conf_med)