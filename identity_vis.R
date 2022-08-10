install.packages("stringr")
library("stringr")
library("ggplot2")

# **Reminder to run data_cleanup.R and identity_scoring.R before starting**

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
ident_M_BIPOC$racegen <- c("BIPOC, Male")
ident_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
ident_M_White$racegen <- c("White, Male")
ident_WNB_White$racegen <- c("White, Women and Non-Binary")
ident_racegen <- rbind(ident_M_White,ident_WNB_White, ident_M_BIPOC,ident_WNB_BIPOC)

#Add column with race identifiers
ident_BIPOC$race <- c("BIPOC")
ident_White$race <- c("White")
ident_race <- rbind(ident_BIPOC,ident_White)


#Box and whisker plots
#Race and gender identity
ggplot(ident_racegen, aes(x=racegen, y=sum_ident)) +
  geom_boxplot()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  theme(axis.text.y = element_text(size=8))+
  ylab("Strength of Geoscience Identity")+
  theme(axis.title.y = element_text(size=14))+
  xlab("")

#Just race
ggplot(ident_race, aes(x=race, y=sum_ident)) +
  geom_boxplot()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  theme(axis.text.y = element_text(size=8))+
  ylab("Strength of Geoscience Identity")+
  theme(axis.title.y = element_text(size=14))+
  xlab("")


#histogram with bars NOT stacked

#Just Race
ggplot(ident_race, aes(x = sum_ident, fill = race)) +
  geom_histogram(position = "identity", alpha = 0.5)+ #with 50% transparency
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity")

ggplot(ident_race, aes(x = sum_ident, fill = race)) +
  geom_histogram(position = "identity")+ #no transparency
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity")
  
#Race and Gender Identity
ggplot(ident_racegen, aes(x = sum_ident, fill = racegen)) +
  geom_histogram(position = "identity", alpha = 0.5)+ #check binwidth
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity, Gender Identity")

ggplot(ident_racegen, aes(x = sum_ident, fill = racegen)) +
  geom_histogram(position = "identity")+ #no transparency
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity, Gender Identity")

#Density curves
#Just race
ggplot(ident_race, aes(x = sum_ident, colour = race)) + 
  geom_density()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity")

#Race and gender identity
ggplot(ident_racegen, aes(x = sum_ident, colour = racegen)) + 
  geom_density()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity, Gender Identity")

  
#Similar to density curves: frequency polygon (similar to histogram)
ggplot(ident_race, aes(x = sum_ident, fill = race)) + 
  geom_freqpoly(binwidth = 4)+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity")
  
  
#Misc and old
#histogram with stacked datasets (not ideal)
hist(x = ident_BIPOC$sum,
     col=rgb(0,0,1,0.2),
     main = "Geoscience identity of BIPOC vs. White students",
     xlim = c(40,110),
     xlab = "Score of Geoscience Identity")
hist(x = ident_White$sum,
     col=rgb(1,0,0,0.2), add=TRUE)
legend('topright', c('BIPOC students', 'White students'),
       fill=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)))

