cov_data <- read.csv2("a6/covid_19_daily_reports_11-21-2021.csv", encoding="UTF-8", header = TRUE)
times_data <- read.table("a6/time_series_covid19_confirmed_11-21-2021.csv", encoding="UTF-8", sep = " ", header = TRUE)

print("Confirmed:")
mean(cov_data$Confirmed)

print("Selbstberechnetes AM:")
sum(cov_data$Confirmed)/nrow(cov_data)

cov_data_ger <- subset(cov_data, Country_Region == "Germany")
print("Germany:")
mean(cov_data_ger$Deaths)

print("Selbstberechnetes AM:")
sum(cov_data_ger$Deaths)/nrow(cov_data_ger)

times_data_ger <- subset(times_data, Country_Region == "Germany")
times_data_ger_nov <- times_data_ger[, grepl("X2021.11", names(times_data_ger))]

creat_vec <- function() {

  vec <- NULL

  for (i in 2:ncol(times_data_ger_nov)) {
    vec[i - 1] <- times_data_ger_nov[i] / times_data_ger_nov[i - 1]
  }

  return(vec)

}

vec_times_data_ger <- creat_vec()

geo_mid <- function() {

  prod(as.numeric(vec_times_data_ger))^(1 / length(vec_times_data_ger))

}

geo_mid()


#============== D) =============

library("maps")

map("world", fill = TRUE, col = "white", bg = "lightblue",
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))


#cov_data_cum <- aggregate(cov_data[c("Deaths", "Confirmed")], by = cov_data["Country_Region"], sum)
cov_data$Color <- "NULL"

assessment <- function() {

  for (i in seq_len(nrow(cov_data))) {
    if (cov_data$Confirmed[i] < 2000) {
      cov_data$Color[i] <- "green"
      print(cov_data$Country_Region[i])
    }else if (cov_data$Confirmed[i] < mean(cov_data$Confirmed)) {
      cov_data$Color[i] <- "orange"
    }else {
      cov_data$Color[i] <- "red"
    }
  }

  return(cov_data)

}

cov_data_ass <- assessment()

points(cov_data_ass$Long_, cov_data_ass$Lat, col = cov_data_ass$Color, pch = 16, cex = 0.5)
print("_________________________________________________________")
