#testing out science identity scoring with just recog,comp,perf

ident_test <- subset(survey_rev,select=c(ident_1:ident_9,ident_15:ident_20))
ident_test <- apply(ident_test,2,as.numeric) #coerce from char to numeric
sum_test <- apply(ident_test,1,sum) #total identity score of each entry
ident_test <- data.frame(cbind(ident_test,sum_test)) #add sum scores as new column, coerce back to dataframe
ident_test <- as.data.frame(cbind(ident_test,demo))

#bin BIPOC and White
ident_test_HispLat <- subset(ident_test, demo_3=="Hispanic or Latino") 
ident_test_BIPOC <- ident_test[grep("Black|Asian|Other|Native|More|I'll", ident_test$demo_4), ]
ident_test_BIPOC <- rbind(ident_test_HispLat,ident_test_BIPOC)
ident_test_White <- subset(ident_test, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#sub-divide White and BIPOC into gender
ident_test_WNB_BIPOC <- ident_test_BIPOC[grep("Female|Non-Binary|Intersex|I'll", ident_test_BIPOC$demo_1), ]
ident_test_WNB_White <- ident_test_White[grep("Female|Non-Binary|Intersex|I'll", ident_test_White$demo_1), ]
ident_test_M_White <- ident_test_White[grep("Male", ident_test_White$demo_1), ]
ident_test_M_BIPOC <- ident_test_BIPOC[grep("Male", ident_test_BIPOC$demo_1), ]

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
ident_test_M_BIPOC$racegen <- c("BIPOC, Male")
ident_test_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
ident_test_M_White$racegen <- c("White, Male")
ident_test_WNB_White$racegen <- c("White, Women and Non-Binary")
ident_test_racegen <- rbind(ident_test_M_White,ident_test_WNB_White, ident_test_M_BIPOC,ident_test_WNB_BIPOC)

#Add column with race identifiers
ident_test_BIPOC$race <- c("BIPOC")
ident_test_White$race <- c("White")
ident_test_race <- rbind(ident_test_BIPOC,ident_test_White)

#difference in identity scores between White and BIPOC?
t.test(x = ident_test_BIPOC$sum,
       y = ident_test_White$sum)

wilcox.test(sum_test ~ race,
            data = ident_test_race,
            exact = FALSE)

#difference in identity scores by race and gender identity?
pairwise.wilcox.test(ident_test_racegen$sum_test, ident_test_racegen$racegen,
                     p.adjust.method = "BH")

#quick vis
ggplot(ident_test_racegen, aes(x = sum_test, y = racegen, fill = racegen)) + 
  geom_density_ridges(size = .5)+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  xlab("Strength of Geoscience Identity")+
  ylab("Gender Identity, Race/Ethnicity")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))