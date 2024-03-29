install.packages("stringr")
install.packages('ggridges')
install.packages('gcookbook')
install.packages('viridis')
install.packages('hrbrthemes')
install.packages('ggsignif')
library("stringr")
library("ggplot2")
library('ggridges')
library('gcookbook')
library('viridis')
library('forcats')
library('hrbrthemes')
library('ggsignif')

# **Reminder to run data_cleanup.R and identity_scoring.R before starting**

#Start chunk to run

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

#for recognition
ident_recog_racegen %>% count(racegen, sort = TRUE)
ident4 <- ident_recog
ident4$racegen <- c("All Responses")
ident4 %>% count(racegen, sort = TRUE)
ident_recog_racegen_plusmean <- rbind(ident4, ident_recog_racegen)

#for performance/competence
ident_perfcomp_racegen %>% count(racegen, sort = TRUE)
ident5 <- ident_perfcomp
ident5$racegen <- c("All Responses")
ident5 %>% count(racegen, sort = TRUE)
ident_perfcomp_racegen_plusmean <- rbind(ident5, ident_perfcomp_racegen)

#End chunk to run

#*** This one for GSA and manuscript:

#Box and whisker plots
#Race and gender identity
ggplot(ident_racegen_plusmean, aes(x=racegen, y=sum_ident, fill=racegen)) +
  geom_boxplot()+
  ggtitle("a)  Geoscience Identity of Senior Geoscience Majors")+
  theme(plot.title = element_text(size=18, face = 'bold', hjust = -0.15))+
  theme(axis.text.x = element_text(size=16, vjust = 1, color='black'))+
  scale_x_discrete(labels = c("All Responses\n(n = 139)",
                              "BIPOC, Female\nand Non-Binary\n(n = 37)", 
                              "BIPOC, Male\n(n = 27)", 
                              "White, Female\nand Non-Binary\n(n = 51)",
                              "White, Male\n(n = 24)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  #scale_y_continuous(limits=c(45,82))+
  ylab("Strength of Geoscience Identity")+ 
  theme(axis.title.y = element_text(size=16))+
  scale_fill_manual(values=c("grey", "#fc766aff", "#fc766aff", "#fc766aff", 
                             "#fc766aff")) +
  theme(legend.position="none")
  #geom_signif(
    #comparisons=list(c("BIPOC, Male","White, Female and Non-Binary"),
                     #c("BIPOC, Male","White, Male")),
    #annotations=c("**","***"),
    #map_signif_level=TRUE, textsize=6,vjust=.5,
    #y_position = c(75, 79)
  #)

#recognition
ggplot(ident_recog_racegen_plusmean, aes(x=racegen, y=sum_recog, fill=racegen)) +
  geom_boxplot()+
  ggtitle("b)  Geoscience Identity: Recognition")+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust = -0.1))+
  theme(axis.text.x = element_text(size=16, vjust = 1,color='black'))+
  scale_x_discrete(labels = c("All Responses\n(n = 139)",
                              "BIPOC, Female\nand Non-Binary\n(n = 37)", 
                              "BIPOC, Male\n(n = 27)", 
                              "White, Female\nand Non-Binary\n(n = 51)",
                              "White, Male\n(n = 24)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  scale_y_continuous(limits=c(20,48))+
  ylab("Strength of Geoscience Identity:\nRecognition")+ 
  theme(axis.title.y = element_text(size=16))+
  scale_fill_manual(values=c("grey", "#fc766aff", "#fc766aff", "#fc766aff", 
                             "#fc766aff")) +
  theme(legend.position="none")

#perfcomp
ggplot(ident_perfcomp_racegen_plusmean, aes(x=racegen, y=sum_perfcomp, fill=racegen)) +
  geom_boxplot()+
  ggtitle("c)  Geoscience Identity: Performance/Competence")+
  theme(plot.title = element_text(size = 18, face = 'bold', hjust = -0.16))+
  theme(axis.text.x = element_text(size=16, vjust = 1,color='black'))+
  scale_x_discrete(labels = c("All Responses\n(n = 139)",
                              "BIPOC, Female\nand Non-Binary\n(n = 37)", 
                              "BIPOC, Male\n(n = 27)", 
                              "White, Female\nand Non-Binary\n(n = 51)",
                              "White, Male\n(n = 24)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=10))+
  #scale_y_continuous(limits=c(12.5,37.5))+
  ylab("Strength of Geoscience Identity:\nPerformance/Competence")+ 
  theme(axis.title.y = element_text(size=16))+
  scale_fill_manual(values=c("grey", "#fc766aff", "#fc766aff", "#fc766aff", 
                             "#fc766aff")) +
  theme(legend.position="none")
  #geom_signif(
    comparisons=list(c("BIPOC, Female and Non-Binary","BIPOC, Male"),
                    c("BIPOC, Male","White, Female and Non-Binary"),
                    c("BIPOC, Female and Non-Binary","White, Male"),
                    c("BIPOC, Male","White, Male"),
                    c("White, Female and Non-Binary","White, Male")),
    annotations=c("*","*","*","*","*"),
    map_signif_level=TRUE, textsize=6,vjust=.5,
    y_position = c(29.5,30.5,33,34.5,36.5)
  )

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
  

#Stacked density curves - shows distributions, but no y-axis for # of students
ggplot(ident_racegen, aes(x = sum_ident, y = racegen, fill = racegen)) + 
  geom_density_ridges(size = .5)+
  scale_fill_viridis_d()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab(expression("Lower Geoscience Identity" %<->% "Higher Geoscience Identity"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
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
  ggtitle("Geoscience Identity: Performance/Competence")+
  xlab(expression("Lower Performance/Competence" %<->% "Higher Performance/Competence"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "none")

#For recognition
ggplot(ident_recog_racegen, aes(x = sum_recog, y = racegen, fill = racegen)) + 
  geom_density_ridges(size = .5)+
  scale_fill_viridis_d()+
  ggtitle("Geoscience Identity: Recognition")+
  xlab(expression("Lower Recognition" %<->% "Higher Recognition"))+
  ylab("Number of Students")+
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  theme(legend.position = "none")



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

#hist little
ident_racegen %>%
  mutate(racegen = fct_reorder(racegen, sum_ident)) %>%
  ggplot(aes(x=sum_ident, color=racegen, fill=racegen)) +
  geom_histogram(alpha=0.6, binwidth = 1) +
  scale_fill_viridis_d() +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    #panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 16)
    ) +
  xlab("Geoscience Identity Score") +
  ylab("Number of students") +
  theme(axis.title.x = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  facet_wrap(~racegen)

#hist with density curve overlaid
ggplot(ident_racegen, aes(x=sum_ident)) + 
  geom_histogram(aes(y=after_stat(density)),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

racegen.labs<-c("BIPOC, Female and Non-Binary (n = 37)", 
                         "BIPOC, Male (n = 27)", 
                         "White, Female and Non-Binary (n = 51)",
                         "White, Male (n = 24)")
names(racegen.labs) <- c("BIPOC, Female and Non-Binary", 
                         "BIPOC, Male", 
                         "White, Female and Non-Binary",
                         "White, Male")

#for manuscript - wrapped histograms to show distributions
ggplot(ident_racegen, (aes(x=sum_ident, color=racegen, fill=racegen))) + 
  geom_histogram(binwidth=1) + 
  scale_fill_viridis_d()+
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  theme(
    legend.position='none',
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
    strip.text.x = element_text(size=16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12)
    )+
  xlab("Geoscience Identity Score") +
  ylab("Number of students") +
  facet_wrap(~racegen,
             labeller = labeller(racegen=racegen.labs))