data <- read.csv2(file = "ü8/covid_19_daily_reports_12-05-2021.csv")
str(data)
norm <- rnorm(nrow(data), mean(data$Confirmed), sd(data$Confirmed))

generateGIF <- function(x) norm[1:(x * 10)]

print(generateGIF(2))

saveGIF({ for (i in 1:30) hist(generateGIF(i)) }, convert =)
