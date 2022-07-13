#dataframe with summed perfcomp section scores and demo info
ident_perfcomp <- subset(survey_rev,select=c(ident_15:ident_20))
ident_perfcomp <- apply(ident_perfcomp,2,as.numeric) #coerce from char to numeric
sum_perfcomp <- apply(ident_perfcomp,1,sum) #total identity score of each entry
ident_perfcomp <- data.frame(cbind(ident_perfcomp,sum_perfcomp)) #add sum scores as new column, coerce back to dataframe
ident_perfcomp <- as.data.frame(cbind(ident_perfcomp,demo))

#test BIPOC and White
ident_perfcomp_HispLat <- subset(ident_perfcomp, demo_3=="Hispanic or Latino") 
ident_perfcomp_BIPOC <- ident_perfcomp[grep("Black|Asian|Other|Native|More|I'll", ident_perfcomp$demo_4), ]
ident_perfcomp_BIPOC <- rbind(ident_perfcomp_HispLat,ident_perfcomp_BIPOC)
ident_perfcomp_White <- subset(ident_perfcomp, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#difference in identity scores between White and BIPOC?
t.test(x = ident_perfcomp_BIPOC$sum,
       y = ident_perfcomp_White$sum)

#Women & Non-binary BIPOC and WNB White?
ident_perfcomp_WNB_BIPOC <- ident_perfcomp_BIPOC[grep("Female|Non-Binary|Intersex", ident_perfcomp_BIPOC$demo_1), ]
ident_perfcomp_WNB_White <- ident_perfcomp_White[grep("Female|Non-Binary|Intersex", ident_perfcomp_White$demo_1), ]
t.test(x = ident_perfcomp_WNB_BIPOC$sum,
       y = ident_perfcomp_WNB_White$sum)

#WNB and Male White?
ident_perfcomp_M_White <- subset(ident_perfcomp_White, demo_1=='Male')
t.test(x = ident_perfcomp_M_White$sum,
       y = ident_perfcomp_WNB_White$sum)

#WNB and Male BIPOC?
ident_perfcomp_M_BIPOC <- subset(ident_perfcomp_BIPOC, demo_1=='Male')
t.test(x = ident_perfcomp_WNB_BIPOC$sum,
       y = ident_perfcomp_M_BIPOC$sum)

#Male BIPOC and Male White?
t.test(x = ident_perfcomp_M_BIPOC$sum,
       y = ident_perfcomp_M_White$sum)

#WNB BIPOC and Male White?
t.test(x = ident_perfcomp_WNB_BIPOC$sum,
       y = ident_perfcomp_M_White$sum)

#Male BIPOC and WNB White?
t.test(x = ident_perfcomp_WNB_White$sum,
       y = ident_perfcomp_M_BIPOC$sum)