#--- Start chunk to run --- 

#dataframe with summed interest section scores and demo info
pers <- subset(survey_rev,select=c(ident_10:ident_14))
pers <- apply(pers,2,as.numeric) #coerce from char to numeric
sum_pers <- apply(pers,1,sum) #total identity score of each entry
pers <- data.frame(cbind(pers,sum_pers)) #add sum scores as new column, coerce back to dataframe
pers <- as.data.frame(cbind(pers,demo))

#bin BIPOC and White
pers_HispLat <- subset(pers, demo_3=="Hispanic or Latino") 
pers_BIPOC <- pers[grep("Black|Asian|Other|Native|More|I'll", pers$demo_4), ]
pers_BIPOC <- rbind(pers_HispLat,pers_BIPOC)
pers_White <- subset(pers, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#sub-divide White and BIPOC into gender
pers_WNB_BIPOC <- pers_BIPOC[grep("Female|Non-Binary|Intersex|I'll", pers_BIPOC$demo_1), ]
pers_WNB_White <- pers_White[grep("Female|Non-Binary|Intersex|I'll", pers_White$demo_1), ]
pers_M_White <- pers_White[grep("Male", pers_White$demo_1), ]
pers_M_BIPOC <- pers_BIPOC[grep("Male", pers_BIPOC$demo_1), ]

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
pers_M_BIPOC$racegen <- c("BIPOC, Male")
pers_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
pers_M_White$racegen <- c("White, Male")
pers_WNB_White$racegen <- c("White, Women and Non-Binary")
pers_racegen <- rbind(pers_M_White,pers_WNB_White, pers_M_BIPOC,pers_WNB_BIPOC)

#Add column with race identifiers
pers_BIPOC$race <- c("BIPOC")
pers_White$race <- c("White")
pers_race <- rbind(pers_BIPOC,pers_White)

#--- End chunk to run ---

#difference in persistence scores between White and BIPOC?
t.test(x = pers_BIPOC$sum_pers,
       y = pers_White$sum_pers)

#difference in persistence scores among race and gender?
pers_racegen$racegen <- factor(pers_racegen$racegen, levels=c("White, Male",
                                                                "White, Women and Non-Binary",
                                                                "BIPOC, Male",
                                                                "BIPOC, Women and Non-Binary"))

racegen.pers.lm <- lm(sum_pers ~ racegen,
                 data = pers_racegen)
summary(racegen.pers.lm)


wilcox.test(sum_pers ~ race,
            data = pers_race,
            exact = FALSE)

#difference in identity scores by race and gender identity?
pairwise.wilcox.test(pers_racegen$sum_pers, pers_racegen$racegen,
                     p.adjust.method = "BH")

#Vis
#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
pers_M_BIPOC$racegen <- c("BIPOC, Male")
pers_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
pers_M_White$racegen <- c("White, Male")
pers_WNB_White$racegen <- c("White, Women and Non-Binary")
pers_racegen <- rbind(pers_M_White,pers_WNB_White, pers_M_BIPOC,pers_WNB_BIPOC)

#Add column with race identifiers
pers_BIPOC$race <- c("BIPOC")
pers_White$race <- c("White")
pers_race <- rbind(pers_BIPOC,pers_White)

#adding mean to plots, n to labels
pers_racegen %>% count(racegen, sort = TRUE)
pers3 <- pers
pers3$racegen <- c("All Responses")
pers3 %>% count(racegen, sort = TRUE)
pers_racegen_plusmean <- rbind(pers3, pers_racegen)
#and for just race
pers2 <- pers
pers2$race <- c("All Responses")
pers_race_plusmean <- rbind(pers2, pers_race)
pers_race %>% count(race, sort = TRUE)

#**same as GSA plots
ggplot(pers_racegen_plusmean, aes(x=racegen, y=sum_pers, fill=racegen)) +
  geom_boxplot()+
  ggtitle("Using and Doing Science")+
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))+
  theme(axis.text.x = element_text(size=14, vjust = 1))+
  scale_x_discrete(labels = c("All Responses\n(n = 167)",
                              "BIPOC, Male\n(n = 27)", 
                              "BIPOC, Female\nand Non-Binary\n(n = 37)", 
                              "White, Male\n(n = 24)",
                              "White, Female\nand Non-Binary\n(n = 51)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=14))+
  ylab("Strength of Using and Doing Science")+ 
  theme(axis.title.y = element_text(size=14))+
  scale_fill_manual(values=c("grey", "#fc766aff", "#fc766aff", "#fc766aff", 
                             "#fc766aff")) +
  theme(legend.position="none")

ggplot(pers_race_plusmean, aes(x=race, y=sum_pers, fill=race)) +
  geom_boxplot()+
  ggtitle("Using and Doing Science")+
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))+
  theme(axis.text.x = element_text(size=14, vjust = 1))+
  scale_x_discrete(labels = c("All Responses\n(n = 167)",
                              "BIPOC\n(n = 68)", 
                              "White\n(n = 78)"))+
  xlab("")+
  theme(axis.text.y = element_text(size=14))+
  ylab("Strength of Using and Doing Science")+
  theme(axis.title.y = element_text(size=14))+
  scale_fill_manual(values=c("grey", "#5b84b1ff", "#5b84b1ff")) +
  theme(legend.position="none")


#Women & Non-binary BIPOC and WNB White?
t.test(x = pers_WNB_BIPOC$sum,
       y = pers_WNB_White$sum) 

#WNB and Male White?
t.test(x = pers_M_White$sum,
       y = pers_WNB_White$sum) 

#WNB and Male BIPOC?
t.test(x = pers_WNB_BIPOC$sum,
       y = pers_M_BIPOC$sum) 

#Male BIPOC and Male White?
t.test(x = pers_M_BIPOC$sum,
       y = pers_M_White$sum)

#WNB BIPOC and Male White?
t.test(x = pers_WNB_BIPOC$sum,
       y = pers_M_White$sum) 

#Male BIPOC and WNB White?
t.test(x = pers_WNB_White$sum,
       y = pers_M_BIPOC$sum)

#quick vis
ggplot(pers_racegen, aes(x = sum_pers, colour = racegen)) + 
  geom_density()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Number of Students")+
  labs(fill = "Race/Ethnicity, Gender Identity")