######### ====================== Loesung zum 8. Aufgabenzettel ====================== #########

#### Aufgabe a) ---- 

# Einlesen des Datensatzes
covid <- read.csv2(file = "ü8/covid_19_daily_reports_12-05-2021.csv", header = TRUE)

# Aufsummieren der einzelnen Regionen der Länder 
sum.countries <- function(df) {
  countries <- unique(df$Country_Region) # Heraussuchen der einzelnen Länder 
  Country = Confirmed = Deaths = c()
  for (i in 1:length(countries)) {
    # Die erste Spalte enthält die Länder 
    Country[i] <- countries[i]
    # Die Spalten für die Fallzahlen werden einfach aufsummiert pro Land 
    # Dies kann auf drei verschiedene Art und Weisen getan werden, die man unten sehen kann: 
    Confirmed[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(subset(df, df$Country_Region == countries[i], select = "Deaths"))
  }
  # Das Ergebnis wird in einen Data Frame geschrieben 
  df_cum <- data.frame(
    Country = Country,
    Confirmed = Confirmed,
    Deaths = Deaths)
  return(df_cum)
}

covid_sum <- sum.countries(covid)

#### ende ---- 

#### Aufgabe b) ---- 

# install.packages("animation")
library(animation)

# Mal fuer spaeter merken ;) Kann man schoene Sachen mit machen! 
?`animation-package`
?saveGIF

#### ende ---- 


#### Aufgabe c) ---- 

?rnorm
?set.seed

set.seed(123)
# So viele Zufallszahlen erstellen, wie der Corona-Datensatz Beobachtungen hat 
# Mittelwert und Standardabweichung ebenfalls von den Corona-Zahlen abgeleitet 
norm <- rnorm(nrow(covid_sum), mean = mean(covid_sum$Confirmed), sd = sd(covid_sum$Confirmed))

#### ende ---- 

#### Aufgabe d) ---- 

amazing_animation <- function(vec, steps, name) {
  # Die Grüßen des Subsets festlegen 
  subset = seq(5, length(vec), steps)
  # Für die Ertsellung der Dichtefunktion wird eine Datengrundlage gebraucht
  # Hier wird das Intervall von -4 bis 4 genommen 
  x <- (seq(-4, 4, length = length(vec)) - mean(vec)) / sd(vec)
  saveGIF(expr = (
    for (i in 1:(length(subset))) {
      hist(vec[1:subset[i]], freq = FALSE, xlab = name)
      curve(dnorm(x, mean(vec), sd(vec)), add = TRUE, col = "red")
    }
  ), movie.name = paste(name, ".gif", sep = ""), convert = "magick")
}

amazing_animation(vec = norm, steps = 10, name = "norm")

# Man sieht, wie sich das Histogramm immer besser der Dichtefunktion anpasst,
# je mehr Beobachtungen für die Erstellung hinzugenommen werden. 

#### ende ---- 


#### Aufgabe e) ---- 

amazing_animation(vec = covid_sum$Confirmed, steps = 10, name = "covid_confirmed")
# Bei den Coronazahlen sieht man, dass diese nicht aussehen wie die Standardnormalverteilung
#### ende ---- 
