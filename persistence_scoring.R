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

#difference in identity scores between White and BIPOC?
t.test(x = pers_BIPOC$sum,
       y = pers_White$sum)

wilcox.test(sum_pers ~ race,
            data = pers_race,
            exact = FALSE)

#difference in identity scores by race and gender identity?
pairwise.wilcox.test(pers_racegen$sum_pers, pers_racegen$racegen,
                     p.adjust.method = "BH")

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