install.packages("stringr")
install.packages('ggridges')
install.packages('gcookbook')
install.packages('viridis')
library("stringr")
library("ggplot2")
library('ggridges')
library('gcookbook')
library('viridis')

# **Reminder to run data_cleanup.R and identity_scoring.R before starting**

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
ident_M_BIPOC$racegen <- c("BIPOC, Male")
ident_WNB_BIPOC$racegen <- c("BIPOC, Female and Non-Binary")
ident_M_White$racegen <- c("White, Male")
ident_WNB_White$racegen <- c("White, Female and Non-Binary")
ident_racegen <- rbind(ident_M_White,ident_WNB_White, ident_M_BIPOC,ident_WNB_BIPOC)

#Add column with race identifiers
ident_BIPOC$race <- c("BIPOC")
ident_White$race <- c("White")
ident_race <- rbind(ident_BIPOC,ident_White)

#adding mean to plots, n to labels
ident_racegen %>% count(racegen, sort = TRUE)
ident3 <- ident
ident3$racegen <- c("All Responses")
ident3 %>% count(racegen, sort = TRUE)
ident_racegen_plusmean <- rbind(ident3, ident_racegen)
#and for just race
ident2 <- ident
ident2$race <- c("All Responses")
ident_race_plusmean <- rbind(ident2, ident_race)
ident_race %>% count(race, sort = TRUE)

#*** This one for GSA:

#Box and whisker plots
#Race and gender identity
ggplot(ident_racegen_plusmean, aes(x=racegen, y=sum_ident, fill=racegen)) +
  geom_boxplot()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))+
  theme(axis.text.x = element_text(size=14, vjust = 1))+
  scale_x_discrete(labels = c("All Responses\n(n = 167)",
                              "BIPOC, Female\nand Non-Binary\n(n = 37)", 
                              "BIPOC, Male\n(n = 27)", 
                              "White, Female\nand Non-Binary\n(n = 51)",
                              "White, Male\n(n = 24)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=14))+
  ylab("Strength of Geoscience Identity")+ 
  theme(axis.title.y = element_text(size=14))+
  scale_fill_manual(values=c("grey", "#fc766aff", "#fc766aff", "#fc766aff", 
                             "#fc766aff")) +
  theme(legend.position="none")

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

#**** this one for GSA
ggplot(ident_race_plusmean, aes(x=race, y=sum_ident, fill=race)) +
  geom_boxplot()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))+
  theme(axis.text.x = element_text(size=14, vjust = 1))+
  scale_x_discrete(labels = c("All Responses\n(n = 167)",
                              "BIPOC\n(n = 68)", 
                              "White\n(n = 78)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=14))+
  ylab("Strength of Geoscience Identity")+
  theme(axis.title.y = element_text(size=14))+
  scale_fill_manual(values=c("grey", "#5b84b1ff", "#5b84b1ff")) +
  theme(legend.position="none")

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

#Race and gender identity - filled
ggplot(ident_racegen, aes(x = sum_ident, fill = racegen, alpha = 0.2)) + 
  geom_density()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity, Gender Identity")

#Race and gender identity - color lines
ggplot(ident_racegen, aes(x = sum_ident, colour = racegen)) + 
  geom_density()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity, Gender Identity")
  
#Similar to density curves: frequency polygon (similar to histogram)
ggplot(ident_race, aes(x = sum_ident, fill = race)) + 
  geom_freqpoly(binwidth = 4)+
  scale_fill_viridis_d()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab(expression("Lower Geoscience Identity" %<->% "Higher Geoscience Identity"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "right")+
  labs(colour = "Race/Ethnicity")
  

#Stacked density curves
ggplot(ident_racegen, aes(x = sum_ident, y = racegen, fill = racegen)) + 
  geom_density_ridges(size = .5)+
  scale_fill_viridis_d()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab(expression("Lower Geoscience Identity" %<->% "Higher Geoscience Identity"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  #theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "none")

ggplot(ident_race, aes(x = sum_ident, y = race, fill = race)) + 
  geom_density_ridges(size = .5)+
  scale_fill_viridis_d()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab(expression("Lower Geoscience Identity" %<->% "Higher Geoscience Identity"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  #theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "none")

#For performance/competence
ggplot(ident_perfcomp_racegen, aes(x = sum_perfcomp, y = racegen, fill = racegen)) + 
  geom_density_ridges(size = .5)+
  scale_fill_viridis_d()+
  ggtitle("Performance/Competence")+
  xlab(expression("Lower Performance/Competence" %<->% "Higher Performance/Competence"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  #theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "none")

#For recognition
ggplot(ident_recog_racegen, aes(x = sum_recog, y = racegen, fill = racegen)) + 
  geom_density_ridges(size = .5)+
  scale_fill_viridis_d()+
  ggtitle("Recognition")+
  xlab(expression("Lower Recognition" %<->% "Higher Recognition"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  #theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "none")


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

