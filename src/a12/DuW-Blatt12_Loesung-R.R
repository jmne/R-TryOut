
#### a) ---- 

# Einlesen des Datensatzes
covid <- read.table(file = "a12/covid_19_daily_reports_01-23-2022.csv",header = TRUE, sep = ";", dec = ".")

# Aufsummieren der einzelnen Regionen der Laender 
sum.countries <- function(df){
  countries <- unique(df$Country_Region) # Heraussuchen der einzelnen Laender 
  Country = Confirmed = Deaths = c()
  for(i in 1:length(countries)){
    # Die erste Spalte enthaelt die Laender 
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

covid_world <- sum.countries(covid)

#### b) ---- 

# Distanzmatrix erstellen, hier ist eine Euklidische Distanz angebracht
## Manhatten-Distanzen sind nicht so anfaellig für Ausreißer
dist_euc <- dist(covid_world$Confirmed, method = "euclidean")

# Erst das Clusterobjekt erstellen (hier immer mit "c..." benannt) und dann mittels der plot-Funktion das Dendrogramm automatisch zeichnen 
## Um das Dendrogramm vernünftig lesen zu können kann es nötig sein, es zu exportieren 
csingle <- hclust(dist_euc, method = "single")
plot(csingle, main = "Single Linkage Clustering", xlab = "Laender-Nummern", ylab = "Distanz")

ccomplete <- hclust(dist_euc, method = "complete")
plot(ccomplete, main = "Complete Linkage Clustering", xlab = "Laender-Nummern", ylab = "Distanz")

caverage <- hclust(dist_euc, method = "average")
plot(caverage, main = "Average Linkage Clustering", xlab = "Laender-Nummern", ylab = "Distanz")

cward <- hclust(dist_euc, method = "ward.D2")
plot(cward, main = "Ward Clustering", xlab = "Laender-Nummern", ylab = "Distanz")

# Die Linkage Verfahren sind relativ aehnlich zueinander, das Ward-Verfahren sieht leicht anders aus. 


# Man erkennt einige Außenseiter, die mal probeweise entfernt werden können, um das Clusterergebnis zu verbessern 
covid_world_outlier <- covid_world[-c(181, 79, 24),]
dist_euc_out <- dist(covid_world_outlier$Confirmed, method = "euclidean")
cward_out <- hclust(dist_euc_out, method = "ward.D2")
plot(cward_out, main = "Ward Clustering", xlab = "Laender-Nummern", ylab = "Distanz")

#### c) ---- 

?hclust 
# Das Cluster-Objekt besteht aus mehreren Variablen, wie unter "Value" zu lesen ist. 
# $order gibt die Reihenfolge an Beobachtungen aus, wie sie im Dendrogramm dargestellt sind 
cward$order 
# $label ist leer, da noch keine Anzahl an Clustern festgelegt wurde 
cward$label 

#### d) ---- 

# Label ordnet jeder Beobachtung das Cluster zu, zu dem es nach dem Clusterergebnis im Dendrogramm gehört 
# Da die Anzahl an Clustern nicht vom Algorithmus, sondern erst nach der Betrachtung des Dendrogramms festgelegt werden kann, 
# muss diese Variable haendisch gefüllt werden. 
# Dafür kann man $order nutzen, da es die Reihenfolge der Beobachtungen im Dendrogramm festlegt 
# Nach Betrachten des Dendrogramms kann die Variable "label" entsprechend gefüllt werden, 
# sodass die Beobachtungen in einem Plot eingefaerbt werden können. 
# Man kann auch label 1 oder 2 zuweisen. Hier werden Farben zugewiesen, damit man das hinterher plotten kann 
## Achtung: Es wurde das Clustering verwendet, nachdem die Außenseiter entfernt wurden. 
# Man kann diese auch in ein eigenes Cluster packen 
#### Man muss abwaegen, was sinvoller ist: Beobachtungen löschen oder ein kleines Cluster erstellen 
cward_out$labels[cward_out$order[1:8]] <- "darkblue"
cward_out$labels[cward_out$order[9:157]] <- "forestgreen"
cward_out$labels[cward_out$order[158:191]] <- "red"

# Danach kann man das Label z.B. nutzen, um das Clusterergebnis zu plotten 
pairs(covid_world_outlier[,2:3], col = cward_out$labels)
# Man erkennt sehr schön die drei Cluster: die Beobachtungen, die naeher an 0 sind und diejenigen, die weiter in den Raum streuen 
# Sinnvolle Bezeichnungen waeren zum Beispiel für Cluster 1 "Laender mit geringen COVID19-Fallzahlen" usw. 

