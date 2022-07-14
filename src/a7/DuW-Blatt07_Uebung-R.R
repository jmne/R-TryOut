######### ====================== Loesung zum 7. Aufgabenzettel ====================== #########

covid <- read.table(file = "a7/covid_19_daily_reports_11-28-2021.csv", header = TRUE, sep = ";", dec = ".")

#### Aufgabe a) ---- 

sum.countries <- function(df) {
  countries <- unique(df$Country_Region)
  Country = Confirmed = Deaths = Recovered = Active = c()
  for (i in 1:length(countries)) {
    Country[i] <- countries[i]
    Confirmed[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df) == "Deaths")])
  }
  df_cum <- data.frame(
    Country = Country,
    Confirmed = Confirmed,
    Deaths = Deaths)
  return(df_cum)
}

covid_count <- sum.countries(covid)

#### ende ---- 


#### Aufgabe b) ---- 

my_median <- function(vector) {
  sorted <- sort(vector)
  n = length(vector)
  if (n %% 2 == 0) {
    my_median = 1 / 2 * (sorted[n / 2] + sorted[n / 2 + 1])
  } else {
    my_median = sorted[(n + 1) / 2]
  }
  return(my_median)
}

my_mean <- function(vector) {
  return((1 / length(vector) * sum(vector)))
}

my_median_deviation <- function(vector) {
  return(sum(abs(vector - my_median(vector))) / length(vector))
}

my_var <- function(vector) {
  var = 0
  n = length(vector)
  for (i in 1:length(vector)) {
    var = var + (vector[i] - my_mean(vector))^2
  }
  var = 1 / (n - 1) * var
  return(var)
}

my_sd <- function(vector) {
  return(sqrt(my_var(vector)))
}

my_md_dev <- my_median_deviation(covid_count$Confirmed)
my_variance <- my_var(covid_count$Confirmed)
my_sdev <- my_sd(covid_count$Confirmed)

#### ende ---- 


#### Aufgabe c) ---- 

library(moments)

confirmed_mean <- my_mean(covid_count$Confirmed)

confirmed_median <- my_median(covid_count$Confirmed)

length(unique(covid_count$Confirmed))
length(covid_count$Confirmed)
covid_count[duplicated(covid_count$Confirmed),]
covid_count[covid_count$Confirmed == 1,]

confirmed_median < confirmed_mean

sk <- moments::skewness(covid_count$Confirmed)

#### ende ---- 
