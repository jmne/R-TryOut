######### ====================== Loesung zum 8. Aufgabenzettel ====================== #########

#### Aufgabe a) ----

# Einlesen des Datensatzes
covid <- read.table(file = "ü9/covid_19_daily_reports_12-12-2021.csv", header = TRUE, sep = ";", dec = ".")
bundeslaender <- read.table(file = "ü9/DE_Bundeslaender.csv", header = TRUE, sep = ";", dec = ".")
states <- read.table(file = "ü9/USA_States.csv", header = TRUE, sep = ";", dec = ".")
world <- read.table(file = "ü9/World_Countries.csv", header = TRUE, sep = ";", dec = ".")

# Angucken der Datensaetze --> Die Klassen der Spalten müssen stimmen, damit wir später damit rechnen können!
str(bundeslaender)
str(states)
str(world)

#### ende ----


#### Aufgabe b) ----

covid_germany <- covid[covid$Country_Region == "Germany",]
covid_us <- covid[covid$Country_Region == "US",]

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

# Aufsummieren der Staaten der USA
sum.states <- function(df) {
  states <- unique(df$Province_State) # Heraussuchen der einzelnen Länder
  State = Confirmed = Deaths = Recovered = Active = c()
  for (i in 1:length(states)) {
    # Die erste Spalte enthält die Länder
    State[i] <- states[i]
    # Die Spalten für die Fallzahlen werden einfach aufsummiert pro Land
    Confirmed[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Deaths")])
  }
  # Das Ergebnis wird in einen Data Frame geschrieben
  df_cum <- data.frame(
    State = State,
    Confirmed = Confirmed,
    Deaths = Deaths)
  return(df_cum)
}

covid_us <- sum.states(covid_us)

#### ende ----

#### Aufgabe c) ----

# Berechnen der relativen Fallzahlen für Deutschland
?merge # Die merge Funktion verbindet die Daten automatisch

names(bundeslaender)[1] <- "Bundesland"
names(states)[1] <- "State"

covid_germany = merge(covid_germany, bundeslaender, by.x = "Province_State", by.y = "Bundesland")
covid_germany$Rel.Conf <- covid_germany$Confirmed / covid_germany$Population
covid_germany$Rel.Deaths <- covid_germany$Deaths / covid_germany$Population

# Alternative: manuell überprüfen, dass die Datensätze zueinander passen
# Berechnen der relativen Fallzahlen für die USA
# ACHTUNG: Filtern nach den Staaten, für die Bevölkerungszahlen vorliegen!
# Danach darauf achten, dass die Staaten gleich sortiert sind, damit man die richtigen Zahlen in Relation setzt!
covid_us <- covid_us[which(covid_us$State %in% states$State),]
all(covid_us$State == states$State)
covid_us$Rel.Conf <- covid_us$Confirmed / states$Population
covid_us$Rel.Deaths <- covid_us$Deaths / states$Population

# Berechnen der relativen Fallzahlen für die Welt analog zu dem Vorgehen für die USA und Deutschland
covid_sum <- covid_sum[which(covid_sum$Country %in% world$Country),]
covid_sum = merge(covid_sum, world, by = "Country")
covid_sum$Rel.Conf <- covid_sum$Confirmed / covid_sum$Population
covid_sum$Rel.Deaths <- covid_sum$Deaths / covid_sum$Population


#### ende ----

#### Aufgabe d) ----

# Man kann der Boxplot-Funktion eine Liste übergeben, sodass diese gemeinsam geplottet werden
boxplot(list(covid_germany$Rel.Conf,
             covid_us$Rel.Conf,
             covid_sum$Rel.Conf),
        xlab = c("Länder"), names = c("Deutschland", "USA", "Welt"),
        ylab = "Bestätigte COVID19-Fälle (relativ zur Bevölkerung)")


# Es geht auch durch das Setzen der Plot-Einstellungen. Nach dem par()-Befehl kann man dann einfach drei getrennte Boxplots erzeugen.
opar = par(mfrow = c(1, 3))
boxplot(covid_germany$Rel.Conf, xlab = "Deutschland", ylab = "Bestätigte COVID19-Fälle (relativ zur Bevölkerung)")
boxplot(covid_us$Rel.Conf, xlab = "USA")
boxplot(covid_sum$Rel.Conf, xlab = "Welt")
par(opar)

# Interpretation Deutschland

ord_ger = covid_germany[order(covid_germany$Rel.Conf),]
ord_ger[1:2,] # Bundesländer mit wenigstens rel. Fallzahlen: Schlesweig-Holstein und Niedersachsen (3% bzw. 5% der Bevölkerung hatten Corona)
ord_ger[nrow(ord_ger),] # Bundesland mit meisten rel. Fallzahlen: Sachsen (14,5% der Bevölkerung hatten Corona)

boxplot(covid_germany$Rel.Conf, plot = FALSE) # Stats für Median und Whisker (Zeile 1, 3 und 5 der stats-Matrix)
quantile(x = covid_germany$Rel.Conf, probs = c(0.25, 0.75)) # 25%:0.06277900, 75%: 0.08523146

# Interpretation USA

ord_usa = covid_us[order(covid_us$Rel.Conf),]
ord_usa[1:3,] # Bundesstaaten mit wenigstens rel. Fallzahlen: Hawaii, Vermont und Oregon (6% - 9% der Bevölkerung hatten Corona )
ord_usa[nrow(ord_usa),] # Bundesstaat mit meisten rel. Fallzahlen: North Dakota (22% der Bevölkerung hatten Corona)

boxplot(ord_usa$Rel.Conf, plot = FALSE) # Stats für Median und Whisker (Zeile 1, 3 und 5 der stats-Matrix)
quantile(x = ord_usa$Rel.Conf, probs = c(0.25, 0.75)) # Quantile


# Interpretation Welt

ord_world = covid_sum[order(covid_sum$Rel.Conf),]
nrow(ord_world)
ord_world[172:174,] # Länder mi den meisten rel. Fallzahlen

boxplot(covid_sum$Rel.Conf, plot = FALSE) # Stats für Median und Whisker (Zeile 1, 3 und 5 der stats-Matrix)
quantile(x = covid_sum$Rel.Conf, probs = c(0.25, 0.75)) # Quantile


#### ende ----

#### Aufgabe e) ----

# Nach einigem Suchen wurde das "ineq" Paket gefunden, dass eine ganz einfach Funktion "lc" hat zum Zeichnen der Lorenzkurve
# install.packages("ineq")
library(ineq)

# Wie beim Boxplot kann man entweder einen Plot machen und Linien hinzufügen oder den par()-Befehl nutzen, um die Fläche zu dritteln

plot(Lc(covid_germany$Rel.Deaths), col = "gold", main = "Lorenzkurve - Relative Todesfälle Deutschland")
lines(Lc(covid_us$Rel.Deaths), col = "darkblue")
lines(Lc(covid_sum$Rel.Deaths), col = "darkred")
legend("topleft", legend = c("Deutschland", "USA", "Welt"), fill = c("gold", "darkblue", "darkred"))

# Beim Welt-Plot ist die Ungleichheit am Größten.

#### ende ----