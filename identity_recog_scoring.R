#dataframe with summed recog section scores and demo info
ident_recog <- subset(survey_rev,select=c(ident_1:ident_9))
ident_recog <- apply(ident_recog,2,as.numeric) #coerce from char to numeric
sum_recog <- apply(ident_recog,1,sum) #total identity score of each entry
ident_recog <- data.frame(cbind(ident_recog,sum_recog)) #add sum scores as new column, coerce back to dataframe
ident_recog <- as.data.frame(cbind(ident_recog,demo))

#test BIPOC and White
ident_recog_HispLat <- subset(ident_recog, demo_3=="Hispanic or Latino") 
ident_recog_BIPOC <- ident_recog[grep("Black|Asian|Other|Native|More|I'll", ident_recog$demo_4), ]
ident_recog_BIPOC <- rbind(ident_recog_HispLat,ident_recog_BIPOC)
ident_recog_White <- subset(ident_recog, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#difference in identity scores between White and BIPOC?
t.test(x = ident_recog_BIPOC$sum,
       y = ident_recog_White$sum)

#Women & Non-binary BIPOC and WNB White?
ident_recog_WNB_BIPOC <- ident_recog_BIPOC[grep("Female|Non-Binary|Intersex", ident_recog_BIPOC$demo_1), ]
ident_recog_WNB_White <- ident_recog_White[grep("Female|Non-Binary|Intersex", ident_recog_White$demo_1), ]
t.test(x = ident_recog_WNB_BIPOC$sum,
       y = ident_recog_WNB_White$sum) 

#WNB and Male White?
ident_recog_M_White <- subset(ident_recog_White, demo_1=='Male')
t.test(x = ident_recog_M_White$sum,
       y = ident_recog_WNB_White$sum)

#WNB and Male BIPOC?
ident_recog_M_BIPOC <- subset(ident_recog_BIPOC, demo_1=='Male')
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