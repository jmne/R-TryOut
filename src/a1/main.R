data <- c(rep(900, 3325),rep(1300, 4527),rep(1500, 2549),rep(2000, 6223),rep(2600, 6332),rep(3200, 4728),rep(4500, 6710),rep(6000, 3494))
histo <- hist(data, breaks = c(0, unique(data)),
              main = "Histogram", xlab = "Kartoffeln", ylab = "Dichte",
              freq = FALSE, probability = TRUE)
skewness(data)
plot(ecdf(data))
abline(v = median(data), col="red")