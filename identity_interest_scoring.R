#dataframe with summed interest section scores and demo info
ident_int <- subset(survey_rev,select=c(ident_10:ident_14))
ident_int <- apply(ident_int,2,as.numeric) #coerce from char to numeric
sum_int <- apply(ident_int,1,sum) #total identity score of each entry
ident_int <- data.frame(cbind(ident_int,sum_int)) #add sum scores as new column, coerce back to dataframe
ident_int <- as.data.frame(cbind(ident_int,demo))

#test BIPOC and White
ident_int_HispLat <- subset(ident_int, demo_3=="Hispanic or Latino") 
ident_int_BIPOC <- ident_int[grep("Black|Asian|Other|Native|More|I'll", ident_int$demo_4), ]
ident_int_BIPOC <- rbind(ident_int_HispLat,ident_int_BIPOC)
ident_int_White <- subset(ident_int, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#difference in identity scores between White and BIPOC?
t.test(x = ident_int_BIPOC$sum,
       y = ident_int_White$sum)

#Women & Non-binary BIPOC and WNB White?
ident_int_WNB_BIPOC <- ident_int_BIPOC[grep("Female|Non-Binary|Intersex", ident_int_BIPOC$demo_1), ]
ident_int_WNB_White <- ident_int_White[grep("Female|Non-Binary|Intersex", ident_int_White$demo_1), ]
t.test(x = ident_int_WNB_BIPOC$sum,
       y = ident_int_WNB_White$sum) 

#WNB and Male White?
ident_int_M_White <- subset(ident_int_White, demo_1=='Male')
t.test(x = ident_int_M_White$sum,
       y = ident_int_WNB_White$sum) 

#WNB and Male BIPOC?
ident_int_M_BIPOC <- subset(ident_int_BIPOC, demo_1=='Male')
t.test(x = ident_int_WNB_BIPOC$sum,
       y = ident_int_M_BIPOC$sum) 

#Male BIPOC and Male White?
t.test(x = ident_int_M_BIPOC$sum,
       y = ident_int_M_White$sum)

#WNB BIPOC and Male White?
t.test(x = ident_int_WNB_BIPOC$sum,
       y = ident_int_M_White$sum) 

#Male BIPOC and WNB White?
t.test(x = ident_int_WNB_White$sum,
       y = ident_int_M_BIPOC$sum)