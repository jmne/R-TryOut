data <- read.csv2("a7/covid_19_daily_reports_11-28-2021.csv", header = TRUE)

sumdata <- function (df){
    countries <- unique(df$Country_Region)
    Country = Confirmed = Deaths = c()
    for(i in 1:length(countries)){
      Country[i] <- countries[i]
      Confirmed[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df)=="Confirmed")])
      Deaths[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df)=="Deaths")])
    }
    return(data.frame(Country,Confirmed,Deaths))

}

s_data <- sumdata(data)

# Median berechnen
my_med <- function(x){
  xmed <- 0
  x <- sort(x)
  if(length(x)%%2 == 0){
    xmed <- (x[length(x)/2]+x[(length(x)/2)+1])/2
  }
  else{
    xmed <- x[(length(x)/2)+1]
  }
  return(xmed)
}

#Mittelwert berechnen
my_mean <- function(x){
  xmean = (1/length(x))*sum(x)
  return(xmean)
}

# Medianabweichung berechnen
my_med_dev <- function(x){
  xmed_dev = (1/length(x))*sum(abs(x-my_med(x)))
  return(xmed_dev)
}

mediAbw<-my_med_dev(cd_agg$Confirmed)

# Varianz berechnen
my_var <- function(x){
  xvar <- (1/(length(x)-1))*sum((x-my_mean(x))^2)
  return(xvar)
}

# Standardabweichung berechnen
my_standabw <- function(x){
  xvar <- sqrt((1/(length(x)-1))*sum((x-my_mean(x))^2))
  return(xvar)
}
"Median"
my_med(s_data$Confirmed)
"Medianabweichung"
my_med_dev(s_data$Confirmed)
"Mittelwert"
my_mean(s_data$Confirmed)
"Varianz"
my_var(s_data$Confirmed)
"Standardabweichung"
my_standabw(s_data$Confirmed)