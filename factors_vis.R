install.packages("stringr")
install.packages('ggridges')
install.packages('gcookbook')
library("stringr")
library("ggplot2")
library('ggridges')
library('gcookbook')
library('viridis')

fact.counts <- read.csv(file = "https://raw.githubusercontent.com/willarowan/identity-survey/main/survey-exports/factors_counts.csv")
#column names come in weird from Github, so:
#names(fact.counts)[1] = 'rating'

#How many respondents in each category?
fact_racegen %>% count(racegen, sort = TRUE)

#Histograms of each factor by race/ethnicity
ggplot(fact_racegen, aes(x = fact_11, color = racegen)) +
  geom_histogram(fill = "white") +
  ggtitle("Feeling Connected to the Earth") +
  xlab("<- Negatively Impacted Geoscience Identity | Positively Impacted Geoscience Identity ->")+
  ylab("Number of Students")+
  guides(color = guide_legend(title = "Race/Ethnicity and Gender"))

#Grouped bar chart of all factors' ratings by race/ethnicity
ggplot(fact.counts, aes(fill = racegen, y = fact_sum, x = rating)) +
  geom_bar(position='dodge', stat='identity')+
  scale_fill_viridis(discrete=TRUE, option='D')+
  ggtitle("Ratings of Factors Influencing Geoscience Identity")+
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  ylab("Frequency of rating")+ 
  theme(axis.title.y = element_text(size=12))+
  labs(fill = "Race/Ethnicity and Gender")

#Grouped bar chart of all factors' ratings by race/ethnicity, percentage
ggplot(fact.counts, aes(fill = racegen, y = percent_total, x = rating)) +
  geom_bar(position='dodge', stat='identity')+
  scale_fill_viridis(discrete=TRUE, option='D')+
  ggtitle("Ratings of Factors Influencing Geoscience Identity")+
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  ylab("Frequency of rating")+ 
  theme(axis.title.y = element_text(size=12))+
  labs(fill = "Race/Ethnicity and Gender")

#Stacked bar chart of all factors' ratings by percentage
ggplot(fact.counts, aes(fill = racegen, y = fact_sum, x = rating)) +
  geom_bar(position='fill', stat='identity')+
  scale_fill_viridis(discrete=TRUE, option='D')+
  ggtitle("Ratings of Factors Influencing Geoscience Identity")+
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  ylab("Frequency of rating")+ 
  theme(axis.title.y = element_text(size=12))+
  labs(fill = "Race/Ethnicity and Gender")

#For just one factor at a time
#Stacked, by percent
ggplot(fact.counts, aes(fill = racegen, y = fact_13, x = rating)) +
  geom_bar(position='fill', stat='identity')+
  scale_fill_viridis(discrete=TRUE, option='D')+
  ggtitle("How did microaggressions affect students' identities as geoscientists?")+
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  ylab("Frequency of rating")+ 
  theme(axis.title.y = element_text(size=12))+
  labs(fill = "Race/Ethnicity and Gender")


##for the manuscript
#Grouped, raw
ggplot(fact.counts, aes(fill = racegen, y = fact_14, x = rating)) +
  geom_bar(position='dodge', stat='identity')+
  ggtitle("Macroaggressions")+
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=12, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=12))+
  ylab("Number of Respondents")+ 
  theme(axis.title.y = element_text(size=14))+
  #scale_fill_discrete()+
  scale_fill_viridis(discrete=TRUE, option='D',
                     labels = c("BIPOC, Female and\nNon-Binary (n = 37)", 
                                "BIPOC, Male (n = 27)", 
                                "White, Female and\nNon-Binary (n = 51)",
                                "White, Male (n = 24)"))+
  labs(fill = "Race/Ethnicity\nand Gender")+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=14))

#connections with Earth?
ggplot(fact.counts, aes(fill = racegen, y = fact_11, x = rating)) +
  geom_bar(position='dodge', stat='identity')+
  scale_fill_viridis(discrete=TRUE, option='D')+
  ggtitle("How did connections with Earth affect students' identities as geoscientists?")+
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  ylab("Frequency of rating")+ 
  theme(axis.title.y = element_text(size=12))+
  labs(fill = "Race/Ethnicity and Gender")

#seeing oneself represented?
ggplot(fact.counts, aes(fill = racegen, y = fact_12, x = rating)) +
  geom_bar(position='dodge', stat='identity')+
  scale_fill_viridis(discrete=TRUE, option='D')+
  ggtitle("How did seeing oneself represented in faculty/staff\naffect students' identities as geoscientists?")+
  theme(plot.title = element_text(size = 14, face = 'bold', hjust = 0))+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = c("Had a strong\nnegative impact",
                              "Had a negative\nimpact", 
                              "Had neither\na positive nor\na negative impact", 
                              "Had a positive\nimpact",
                              "Had a strong\npositive impact",
                              "Not applicable/\nDid not\nexperience this"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  ylab("Frequency of rating")+ 
  theme(axis.title.y = element_text(size=12))+
  labs(fill = "Race/Ethnicity and Gender")
