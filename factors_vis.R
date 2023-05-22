install.packages("stringr")
install.packages('ggridges')
install.packages('gcookbook')
library("stringr")
library("ggplot2")
library('ggridges')
library('gcookbook')

#Histograms of each factor by race/ethnicity
ggplot(fact_racegen, aes(x = fact_11, color = racegen)) +
  geom_histogram(fill = "white") +
  ggtitle("Feeling Connected to the Earth") +
  xlab("<- Negatively Impacted Geoscience Identity | Positively Impacted Geoscience Identity ->")+
  ylab("Number of Students")+
  guides(color = guide_legend(title = "Race/Ethnicity"))