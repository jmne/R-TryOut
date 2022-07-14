######### ====================== Loesung zum 6. Aufgabenzettel ====================== #########

#### Aufgabe a) ---- 

covid_daily <- read.table(file = "covid_19_daily_reports_11-21-2021.csv", header = TRUE, sep = ";", dec = ".")

covid_ts <- read.table(file = "time_series_covid19_confirmed_11-21-2021.csv", header = TRUE, sep = " ", dec = ".")

# Anschauen des Datensatzes: 
str(covid_daily)
summary(covid_daily)
str(covid_ts)
summary(covid_ts)

#### ende ---- 


#### Aufgabe b) ---- 
# Filtern nach den Werten fuer Deutschland 
covid_daily_germany <- covid_daily[covid_daily$Country_Region == "Germany",]

my_mean <- function(vector) {
  return(sum(vector) / length(vector))
}

# Teste die eigene mean-Funktion
my_mean(c(1, 2, 3)) == mean(c(1, 2, 3))

# Mittelwert berechnen fuer die geforderten Werte: Durchschnittswerte pro Bundesland  
mean_confirmed <- my_mean(covid_daily_germany$Confirmed) # 336602.4
mean_deaths <- my_mean(covid_daily_germany$Deaths) # 6195.625


#### ende ---- 

#### Aufgabe c) ---- 
# Wir nehmen nun den anderen Datensatz zur Hand und filtern ihn nach den Eintraegen für Deutschland

index_germany = which(covid_ts$Country_Region == "Germany")
covid_ts_ger <- covid_ts[index_germany,]

#Nun loeschen wir die Spalten, die die Laenderbezeichnung innehaben und suchen nach den Einträgen für November 
covid_ts_ger <- covid_ts_ger[, -1]
covid_ts_ger_nov = covid_ts_ger[, grepl("X2021.11", names(covid_ts_ger))]

growth_factor <- function(df) {
  c = c(1)
  i = 1
  while (i < ncol(df)) {
    c[i + 1] <- 1 + (df[1, i + 1] - df[1, i]) / df[1, i]
    i = i + 1
  }
  return(c)
}

# In Base-R gibt es keine Funktion, um die N-te Wurzel zu ziehen. Deswehen muss ein extra Paket geladen werden 
install.packages("pracma")
library(pracma)

# Definieren des geometrischen Mittelwerts 
my_geom_mean <- function(vector) {
  return(pracma::nthroot(prod(growth_factor(vector)), length(vector))) }

geom_mean1 <- my_geom_mean(covid_ts_ger_nov) # 1.0115 -> Durchschnittlich +1.1 % 

# Nur zur Kontrolle (nicht in Aufgabenstellung enthalten gewesen)
install.packages("EnvStats")
library(EnvStats)

geom_mean2 <- EnvStats::geoMean(growth_factor(covid_ts_ger_nov))

geom_mean1 == geom_mean2 # Es kommt FALSE heraus, da die beiden Funktionen intern anders runden 
round(geom_mean1, 8) == round(geom_mean2, 8) # Wennn man auf acht Stellen nach dem Komma rundet, kommen beide aber auf dasselbe Ergebnis

#### ende ---- 


#### Aufgabe d) ---- 

install.packages("maps")
library(maps)

assessment <- function(df) {
  for (i in 1:nrow(df)) {
    if (df$Confirmed[i] < 2000) {
      df$assessment[i] <- "green"
      next()
    }
    else if (df$Confirmed[i] < mean(df$Confirmed)) {
      df$assessment[i] <- "orange"
      next()
    }
    else {
      df$assessment[i] <- "red"
    }
  }
  return(df)
}

covid_assessment <- assessment(covid_daily)

map("world", fill = TRUE, col = "white", bg = "lightblue", ylim = c(-60, 90), mar = c(0, 0, 0, 0))

points(covid_assessment$Long_, covid_assessment$Lat, col = covid_assessment$assessment, pch = 16, cex = 0.5)

#### ende ---- 
