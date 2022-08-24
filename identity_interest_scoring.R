#--- Start chunk to run --- 

#dataframe with summed interest section scores and demo info
ident_int <- subset(survey_rev,select=c(ident_10:ident_14))
ident_int <- apply(ident_int,2,as.numeric) #coerce from char to numeric
sum_int <- apply(ident_int,1,sum) #total identity score of each entry
ident_int <- data.frame(cbind(ident_int,sum_int)) #add sum scores as new column, coerce back to dataframe
ident_int <- as.data.frame(cbind(ident_int,demo))

#bin BIPOC and White
ident_int_HispLat <- subset(ident_int, demo_3=="Hispanic or Latino") 
ident_int_BIPOC <- ident_int[grep("Black|Asian|Other|Native|More|I'll", ident_int$demo_4), ]
ident_int_BIPOC <- rbind(ident_int_HispLat,ident_int_BIPOC)
ident_int_White <- subset(ident_int, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#sub-divide White and BIPOC into gender
ident_int_WNB_BIPOC <- ident_int_BIPOC[grep("Female|Non-Binary|Intersex|I'll", ident_int_BIPOC$demo_1), ]
ident_int_WNB_White <- ident_int_White[grep("Female|Non-Binary|Intersex|I'll", ident_int_White$demo_1), ]
ident_int_M_White <- ident_int_White[grep("Male", ident_int_White$demo_1), ]
ident_int_M_BIPOC <- ident_int_BIPOC[grep("Male", ident_int_BIPOC$demo_1), ]

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
ident_int_M_BIPOC$racegen <- c("BIPOC, Male")
ident_int_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
ident_int_M_White$racegen <- c("White, Male")
ident_int_WNB_White$racegen <- c("White, Women and Non-Binary")
ident_int_racegen <- rbind(ident_int_M_White,ident_int_WNB_White, ident_int_M_BIPOC,ident_int_WNB_BIPOC)

#Add column with race identifiers
ident_int_BIPOC$race <- c("BIPOC")
ident_int_White$race <- c("White")
ident_int_race <- rbind(ident_int_BIPOC,ident_int_White)

#--- End chunk to run ---

#difference in identity scores between White and BIPOC?
t.test(x = ident_int_BIPOC$sum,
       y = ident_int_White$sum)

wilcox.test(sum_int ~ race,
            data = ident_int_race,
            exact = FALSE)

#difference in identity scores by race and gender identity?
pairwise.wilcox.test(ident_int_racegen$sum_int, ident_int_racegen$racegen,
                     p.adjust.method = "BH")

#Women & Non-binary BIPOC and WNB White?
t.test(x = ident_int_WNB_BIPOC$sum,
       y = ident_int_WNB_White$sum) 

#WNB and Male White?
t.test(x = ident_int_M_White$sum,
       y = ident_int_WNB_White$sum) 

#WNB and Male BIPOC?
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