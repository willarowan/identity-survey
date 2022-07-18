fact <- survey_rev[,grepl("^fact",names(survey_rev))] #just factors score columns
demo <- survey_rev[,grepl("^demo",names(survey_rev))] #just demo info
fact <- apply(fact,2,as.numeric, ra.rm=TRUE) #coerce from char to numeric
sum_fact <- apply(fact,1,sum) #total factor score of each entry
fact <- data.frame(cbind(fact,sum_fact)) #add sum scores as new column, coerce back to dataframe
fact <- as.data.frame(cbind(fact,demo)) #bind with demo, coerce to dataframe

#BIPOC and White subgroups
fact_HispLat <- subset(fact, demo_3=="Hispanic or Latino") 
fact_BIPOC <- fact[grep("Black|Asian|Other|Native|More|I'll", fact$demo_4), ]
fact_BIPOC <- rbind(fact_HispLat,fact_BIPOC)
fact_White <- subset(fact, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#research outside class?
t.test(x = fact_BIPOC$fact_4,
       y = fact_White$fact_4)

t.test(x = fact_WNB_BIPOC$fact_4,
       y = fact_M_White$fact_4)

t.test(x = fact_M_BIPOC$fact_4,
       y = fact_M_White$fact_4)

#field experiences?
t.test(x = fact_BIPOC$fact_7,
       y = fact_White$fact_7)

t.test(x = fact_WNB_BIPOC$fact_7,
       y = fact_M_White$fact_7)

t.test(x = fact_M_BIPOC$fact_7,
       y = fact_M_White$fact_7)

#knowledge of careers?
t.test(x = fact_BIPOC$fact_8,
       y = fact_White$fact_8)

#support from fam?
t.test(x = fact_BIPOC$fact_9,
       y = fact_White$fact_9)

#feeling a sense of belonging?
t.test(x = fact_BIPOC$fact_10,
       y = fact_White$fact_10)

fact_WNB_BIPOC <- fact_BIPOC[grep("Female|Non-Binary|Intersex|I'll", fact_BIPOC$demo_1), ]
fact_WNB_White <- fact_White[grep("Female|Non-Binary|Intersex|I'll", fact_White$demo_1), ]
fact_M_White <- fact_White[grep("Male", fact_White$demo_1), ]
t.test(x = fact_M_White$fact_10,
       y = fact_WNB_White$fact_10)

fact_M_BIPOC <- fact_BIPOC[grep("Male", fact_BIPOC$demo_1), ]
t.test(x = fact_WNB_BIPOC$fact_10,
       y = fact_M_BIPOC$fact_10)

t.test(x = fact_M_BIPOC$fact_10,
       y = fact_M_White$fact_10)

t.test(x = fact_WNB_BIPOC$fact_10,
       y = fact_M_White$fact_10)

t.test(x = fact_WNB_BIPOC$fact_10,
       y = fact_WNB_White$fact_10)

t.test(x = fact_M_BIPOC$fact_10,
       y = fact_WNB_White$fact_10)

#seeing oneself represented?
t.test(x = fact_BIPOC$fact_12,
       y = fact_White$fact_12)

t.test(x = fact_WNB_BIPOC$fact_12,
       y = fact_M_White$fact_12)

#microaggressions?
t.test(x = fact_BIPOC$fact_13,
       y = fact_White$fact_13)

t.test(x = fact_WNB_BIPOC$fact_13,
       y = fact_M_White$fact_13) #significant

#macroagressions?
t.test(x = fact_BIPOC$fact_14,
       y = fact_White$fact_14)

t.test(x = fact_WNB_BIPOC$fact_14,
       y = fact_M_White$fact_14)