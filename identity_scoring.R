#Sum identity score
ident <- survey_rev[,grepl("^ident",names(survey_rev))] #just identity score columns
demo <- survey_rev[,grepl("^demo",names(survey_rev))] #just demo info
ident <- apply(ident,2,as.numeric) #coerce from char to numeric
sum <- apply(ident,1,sum) #total identity score of each entry
ident <- data.frame(cbind(ident,sum)) #add sum scores as new column, coerce back to dataframe
ident <- as.data.frame(cbind(ident,demo)) #bind with demo, coerce to dataframe

#total identity score by racial identity
aggregate(formula = sum ~ demo_4,
          FUN = mean,
          data = ident)
#alternate which also works
ident %>% # using dplyr
  group_by(demo_4) %>%
  summarise(avg_sum = mean(sum, na.rm=TRUE))

#total identity score by gender identity
aggregate(formula = sum ~ demo_1,
          FUN = mean,
          data = ident)

#checking for statistical significance
t.test(formula = sum ~ demo_1,
       data = ident,
       subset = demo_1 %in% c('Female', 'Male'))

aov(formula = sum ~ demo_4,
               data = ident)

#test dataframe with binning into BIPOC
?grep
ident_HispLat <- subset(ident, demo_3=="Hispanic or Latino") 
ident_BIPOC <- ident[grep("Black|Asian|Other|Native|More|I'll", ident$demo_4), ]
ident_BIPOC <- rbind(ident_HispLat,ident_BIPOC)
ident_White <- subset(ident, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#difference in identity scores between White and BIPOC?
t.test(x = ident_BIPOC$sum,
       y = ident_White$sum)

#Women & Non-binary BIPOC and WNB White?
ident_WNB_BIPOC <- ident_BIPOC[grep("Female|Non-Binary|Intersex", ident_BIPOC$demo_1), ]
ident_WNB_White <- ident_White[grep("Female|Non-Binary|Intersex", ident_White$demo_1), ]
t.test(x = ident_WNB_BIPOC$sum,
       y = ident_WNB_White$sum) 

#WNB and Male White?
ident_M_White <- subset(ident_White, demo_1=='Male')
t.test(x = ident_M_White$sum,
       y = ident_WNB_White$sum)

#WNB and Male BIPOC?
ident_M_BIPOC <- subset(ident_BIPOC, demo_1=='Male')
t.test(x = ident_WNB_BIPOC$sum,
       y = ident_M_BIPOC$sum)

#Male BIPOC and Male White?
t.test(x = ident_M_BIPOC$sum,
       y = ident_M_White$sum)

#WNB BIPOC and Male White?
t.test(x = ident_WNB_BIPOC$sum,
       y = ident_M_White$sum)

#Male BIPOC and WNB White?
t.test(x = ident_WNB_White$sum,
       y = ident_M_BIPOC$sum)
