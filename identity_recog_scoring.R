install.packages('WRS2')
library(WRS2)

#begin chunk to run

#dataframe with summed recog section scores and demo info
ident_recog <- subset(survey_rev,select=c(ident_1:ident_9))
ident_recog <- apply(ident_recog,2,as.numeric) #coerce from char to numeric
sum_recog <- apply(ident_recog,1,sum) #total identity score of each entry
#add sum scores as new column, coerce back to dataframe
ident_recog <- data.frame(cbind(ident_recog,sum_recog)) 
ident_recog <- as.data.frame(cbind(ident_recog,demo))

#test BIPOC and White
ident_recog_HispLat <- subset(ident_recog, demo_3=="Hispanic or Latino") 
ident_recog_BIPOC <- ident_recog[grep("Black|Asian|Other|Native|More|I'll", 
                                      ident_recog$demo_4), ]
ident_recog_BIPOC <- rbind(ident_recog_HispLat,ident_recog_BIPOC)
ident_recog_White <- subset(ident_recog, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#race and gender identity groups
ident_recog_WNB_BIPOC <- ident_recog_BIPOC[grep("Female|Non-Binary|Intersex", 
                                                ident_recog_BIPOC$demo_1), ]
ident_recog_WNB_White <- ident_recog_White[grep("Female|Non-Binary|Intersex", 
                                                ident_recog_White$demo_1), ]
ident_recog_M_White <- subset(ident_recog_White, demo_1=='Male')
ident_recog_M_BIPOC <- subset(ident_recog_BIPOC, demo_1=='Male')
ident_recog_M_BIPOC$racegen <- c("BIPOC, Male")
ident_recog_WNB_BIPOC$racegen <- c("BIPOC, Female and Non-Binary")
ident_recog_M_White$racegen <- c("White, Male")
ident_recog_WNB_White$racegen <- c("White, Female and Non-Binary")
ident_recog_racegen <- rbind(ident_recog_M_White,ident_recog_WNB_White, 
                       ident_recog_M_BIPOC,ident_recog_WNB_BIPOC)
ident_recog_racegen.forboot <- rbind(ident_recog_WNB_BIPOC, ident_recog_M_BIPOC, ident_recog_WNB_White, ident_recog_M_White)

#end chunk to run

#descriptive stats
ident_recog_racegen %>% # using dplyr
  #group_by(racegen) %>%
  summarise(avg_sum_recog = mean(sum_recog, na.rm=TRUE))

#bootstrap ANOVA
boot.recog.racegen <- t1waybt(sum_recog~racegen,tr=.2,nboot=4999, 
                              data=ident_recog_racegen)
hist(boot.recog.racegen$test)

#pairwise post-hoc tests
bootpairwise.recog.racegen <- mcppb20(sum_recog~racegen,tr=.2,nboot=9999, 
                                      data=ident_recog_racegen)

##### old

#difference in identity scores between White and BIPOC?
t.test(x = ident_recog_BIPOC$sum,
       y = ident_recog_White$sum)

#Women & Non-binary BIPOC and WNB White?
t.test(x = ident_recog_WNB_BIPOC$sum,
       y = ident_recog_WNB_White$sum) 

#WNB and Male White?
t.test(x = ident_recog_M_White$sum,
       y = ident_recog_WNB_White$sum)

#WNB and Male BIPOC?
t.test(x = ident_recog_WNB_BIPOC$sum,
       y = ident_recog_M_BIPOC$sum)

#Male BIPOC and Male White?
t.test(x = ident_recog_M_BIPOC$sum,
       y = ident_recog_M_White$sum)

#WNB BIPOC and Male White?
t.test(x = ident_recog_WNB_BIPOC$sum,
       y = ident_recog_M_White$sum)

#Male BIPOC and WNB White?
t.test(x = ident_recog_WNB_White$sum,
       y = ident_recog_M_BIPOC$sum)