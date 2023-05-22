install.packages("stringr")
install.packages('ggridges')
install.packages('gcookbook')
library("stringr")
library("ggplot2")
library('ggridges')
library('gcookbook')

fact.counts <- read.csv("factors_counts.csv")

#Histograms of each factor by race/ethnicity
ggplot(fact_racegen, aes(x = fact_11, color = racegen)) +
  geom_histogram(fill = "white") +
  ggtitle("Feeling Connected to the Earth") +
  xlab("<- Negatively Impacted Geoscience Identity | Positively Impacted Geoscience Identity ->")+
  ylab("Number of Students")+
  guides(color = guide_legend(title = "Race/Ethnicity and Gender"))

#Grouped bar chart of all factors' ratings by race/ethnicity
ggplot(fact.counts, aes(fill = racegen, y = fact_sum, x = rating)) +
  geom_bar(position='dodge', stat='identity')

#Grouped bar chart of all factors' ratings by race/ethnicity, percentage
ggplot(fact.counts, aes(fill = racegen, y = percent_total, x = rating)) +
  geom_bar(position='dodge', stat='identity')

#Stacked bar chart of all factors' ratings by percentage
ggplot(fact.counts, aes(fill = racegen, y = fact_sum, x = rating)) +
  geom_bar(position='fill', stat='identity')

#For just one factor at a time
#Stacked, by percent
ggplot(fact.counts, aes(fill = racegen, y = fact_13, x = rating)) +
  geom_bar(position='fill', stat='identity')
#Grouped, raw
ggplot(fact.counts, aes(fill = racegen, y = fact_13, x = rating)) +
  geom_bar(position='dodge', stat='identity')
