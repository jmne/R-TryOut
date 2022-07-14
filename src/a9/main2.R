cov <- read.csv2("a9/covid_19_daily_reports_12-12-2021.csv", header = TRUE)
world <- read.csv2("a9/World_Countries.csv", header = TRUE)
de <- read.csv2("a9/DE_Bundeslaender.csv", header = TRUE)
usa <- read.csv2("a9/USA_States.csv", header = TRUE)

agg_world <- function(df) {

  Countries <- unique(df$Country_Region)
  Confirmed <- Deaths <- c()

  for (i in 1:length(Countries)) {

    print(Countries[i])
    Confirmed[i] <- sum(df[df$Country_Region == Countries[i], which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(df[df$Country_Region == Countries[i], which(colnames(df) == "Deaths")])

  }

  return(data.frame(
    Countries,
    Confirmed,
    Deaths
  ))

}

cov_sum <- agg_world(cov)

# ======= Agg. USA =======

agg_pop <- function(df) {
  return(sum(df$Population))
}

pop_usa <- agg_pop(usa)
pop_de <- agg_pop(de)
pop_world <- agg_pop(world)

names(de) <- c("Bundesland", "Population")

de_merged <- merge(cov, de, by.x = "Province_State", by.y = "Bundesland")

de_merged$Relatives_Death <- "None"
names(de_merged) <- c("Province_State", "Relatives_Conf", "Confirmed", "Deaths", "Population", "Relatives_Death")

for (i in 1:nrow(de_merged)) {
  de_merged[i, "Relatives_Conf"] <- de_merged[i, "Confirmed"] / de_merged[i, "Population"]
  de_merged[i, "Relatives_Death"] <- de_merged[i, "Deaths"] / de_merged[i, "Population"]
}

boxplot(as.numeric(de_merged$Relatives_Death), col = c("red", "green", "blue"))
title("Relative confirmed COVID Cases")