data <- read.csv2("a7/covid_19_daily_reports_11-28-2021.csv", header = TRUE)

sd <- aggregate(x = data[c("Deaths", "Confirmed")], by = data["Country_Region"], sum)

agg <- function(df) {
  countries <- unique(df["Country_Region"])
  Confirmed <- Country <- Deaths <- NULL
  for (i in 1:nrow(countries)) {
    Country[i] <- countries[i, 1]
    Confirmed[i] <- sum(df[df$Country_Region == countries[i, 1], which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(df[df$Country_Region == countries[i, 1], which(colnames(df) == "Deaths")])
  }
  return(data.frame(
    Country,
    Confirmed,
    Deaths
  ))
}

sdl <- agg(data)

#==========================   b)    ============================

n <- nrow(sdl)


medx <- function(df) {

  df <- sort(x = df$Confirmed)
  if (n %% 2 == 0) {
    medn <- (df[floor(n / 2)] + df[ceiling(n / 2)]) / 2
  }else {
    medn <- df[ceiling(n / 2)]
  }

}

print(medx(sdl))
median(sdl$Confirmed)

#== Varianz ==

func_v <- function(x) {
  (x - sum(sdl$Confirmed) / n)^2
}

x <- 0

for (i in 1:n) {
  x <- x + func_v(sdl[i, "Confirmed"])
}
vari <- x / (n - 1)

# ==== Stand. Abwei. ====

sqrt(vari)

# === c) ===

# Mittelwert:

mw <- sum(sdl$Confirmed) / n
med <- medx(sdl)

drawing <- c(mw, med)

pie(drawing, labels = c("Mittelwert", "Median"))

hist(sdl$Confirmed, main = "Histogram", col = "steelblue", xlab = "daten", ylab = "Aufkommen", breaks = seq(0, max(sdl$Confirmed), length = 1000))
box()
library("moments")
skewness(sdl$Confirmed)