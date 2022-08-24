#--- Start chunk to run ---

#dataframe with summed perfcomp section scores and demo info
ident_perfcomp <- subset(survey_rev,select=c(ident_15:ident_20))
ident_perfcomp <- apply(ident_perfcomp,2,as.numeric) #coerce from char to numeric
sum_perfcomp <- apply(ident_perfcomp,1,sum) #total identity score of each entry
ident_perfcomp <- data.frame(cbind(ident_perfcomp,sum_perfcomp)) #add sum scores as new column, coerce back to dataframe
ident_perfcomp <- as.data.frame(cbind(ident_perfcomp,demo))

#bin BIPOC and White
ident_perfcomp_HispLat <- subset(ident_perfcomp, demo_3=="Hispanic or Latino") 
ident_perfcomp_BIPOC <- ident_perfcomp[grep("Black|Asian|Other|Native|More|I'll", ident_perfcomp$demo_4), ]
ident_perfcomp_BIPOC <- rbind(ident_perfcomp_HispLat,ident_perfcomp_BIPOC)
ident_perfcomp_White <- subset(ident_perfcomp, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#sub-divide White and BIPOC into gender
ident_perfcomp_WNB_BIPOC <- ident_perfcomp_BIPOC[grep("Female|Non-Binary|Intersex|I'll", ident_perfcomp_BIPOC$demo_1), ]
ident_perfcomp_WNB_White <- ident_perfcomp_White[grep("Female|Non-Binary|Intersex|I'll", ident_perfcomp_White$demo_1), ]
ident_perfcomp_M_White <- ident_perfcomp_White[grep("Male", ident_perfcomp_White$demo_1), ]
ident_perfcomp_M_BIPOC <- ident_perfcomp_BIPOC[grep("Male", ident_perfcomp_BIPOC$demo_1), ]

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
ident_perfcomp_M_BIPOC$racegen <- c("BIPOC, Male")
ident_perfcomp_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
ident_perfcomp_M_White$racegen <- c("White, Male")
ident_perfcomp_WNB_White$racegen <- c("White, Women and Non-Binary")
ident_perfcomp_racegen <- rbind(ident_perfcomp_M_White,ident_perfcomp_WNB_White, ident_perfcomp_M_BIPOC,ident_perfcomp_WNB_BIPOC)

#Add column with race identifiers
ident_perfcomp_BIPOC$race <- c("BIPOC")
ident_perfcomp_White$race <- c("White")
ident_perfcomp_race <- rbind(ident_perfcomp_BIPOC,ident_perfcomp_White)

#--- End chunk to run --- 

#difference in identity scores between White and BIPOC?
t.test(x = ident_perfcomp_BIPOC$sum,
       y = ident_perfcomp_White$sum)

wilcox.test(sum_perfcomp ~ race,
            data = ident_perfcomp_race,
            exact = FALSE)

#difference in identity scores by race and gender identity?
pairwise.wilcox.test(ident_perfcomp_racegen$sum_perfcomp, ident_perfcomp_racegen$racegen,
                     p.adjust.method = "BH")

#Women & Non-binary BIPOC and WNB White?
t.test(x = ident_perfcomp_WNB_BIPOC$sum,
       y = ident_perfcomp_WNB_White$sum)

#WNB and Male White?
t.test(x = ident_perfcomp_M_White$sum,
       y = ident_perfcomp_WNB_White$sum)

#WNB and Male BIPOC?
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