#Sum identity score
ident <- survey_rev[,grepl("^ident",names(survey_rev))] #just identity score columns
demo <- survey_rev[,grepl("^demo",names(survey_rev))] #just demo info
ident <- apply(ident,2,as.numeric) #coerce from char to numeric
sum_ident <- apply(ident,1,sum) #total identity score of each entry
ident <- data.frame(cbind(ident,sum_ident)) #add sum scores as new column, coerce back to dataframe
ident <- as.data.frame(cbind(ident,demo)) #bind with demo, coerce to dataframe

#can the above be turned into a function?
sum.ident.score <- function(data){
  ident <- data[,grepl("^ident",names(data))]
  demo <- data[,grepl("^demo",names(data))] #just demo info
  ident <- apply(ident,2,as.numeric) #coerce from char to numeric
  sum <- apply(ident,1,sum) #total identity score of each entry
  ident <- data.frame(cbind(ident,sum)) #add sum scores as new column, coerce back to dataframe
  ident <- as.data.frame(cbind(ident,demo))
  return(ident)
}
ident<- sum.ident.score(survey_rev) # that works

#Above funtion, but customizable for anything I want to index in the survey?
#(B/c above fuction just pulls full identity score)

#Working code for dataframe with summed interest section scores and demo info
#I want to make this a function, and be able to change which columns I subset
ident_int <- subset(survey_rev,select=c(ident_10:ident_14)) #interest section
demo <- survey_rev[,grepl("^demo",names(survey_rev))]
ident_int <- apply(ident_int,2,as.numeric) #coerce from char to numeric
sum_int <- apply(ident_int,1,sum) #total identity score of each entry
ident_int <- data.frame(cbind(ident_int,sum_int)) #add sum scores as new column, coerce back to dataframe
ident_int <- as.data.frame(cbind(ident_int,demo)) 

#trying as a function, argument 'index' for columns to subset
sum.score <- function(data, index){
  ident <- subset(data,select=c(index))
  demo <- data[,grepl("^demo",names(data))]
  ident <- apply(ident,2,as.numeric) #coerce from char to numeric
  sum <- apply(ident,1,sum) #total identity score of each entry
  ident <- data.frame(cbind(ident,sum)) #add sum scores as new column, coerce back to dataframe
  ident <- as.data.frame(cbind(ident,demo))
  return(ident)
}
#trying function with specifying interest section columns
ident_int <- sum.score(survey_rev, survey_rev[,ident_10:ident_14]) #doesn't work


#statistics time
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
ident_WNB_BIPOC <- ident_BIPOC[grep("Female|Non-Binary|Intersex|I'll", ident_BIPOC$demo_1), ]
ident_W_BIPOC <- ident_BIPOC[grep("Female", ident_BIPOC$demo_1), ]
ident_NB_BIPOC <- ident_BIPOC[grep("Non-Binary|Intersex|I'll", ident_BIPOC$demo_1), ]
ident_W_White <- ident_White[grep("Female", ident_White$demo_1), ]
ident_NB_White <- ident_White[grep("Non-Binary|Intersex|I'll", ident_White$demo_1), ]
ident_WNB_White <- ident_White[grep("Female|Non-Binary|Intersex|I'll", ident_White$demo_1), ]
t.test(x = ident_WNB_BIPOC$sum,
       y = ident_WNB_White$sum) 
t.test(x = ident_NB_BIPOC$sum,
       y = ident_NB_White$sum)

#WNB and Male White?
ident_M_White <- ident_White[grep("Male", ident_White$demo_1), ]
t.test(x = ident_M_White$sum,
       y = ident_WNB_White$sum)

#WNB and Male BIPOC?
ident_M_BIPOC <- ident_BIPOC[grep("Male", ident_BIPOC$demo_1), ]
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

#WNB and Male?
ident_M <- subset(ident, demo_1=='Male')
ident_WNB <- ident[grep("Female|Non-Binary|Intersex", ident$demo_1), ]
t.test(x = ident_WNB$sum,
       y = ident_M$sum)

#regression
lm(formula = ident_BIPOC$sum ~ ident_White$sum)

#plotting results
hist(x = ident_BIPOC$sum,
     col=rgb(0,0,1,0.2),
     main = "Geoscience identity of BIPOC vs. White students",
     xlim = c(40,110),
     xlab = "Score of Geoscience Identity")
hist(x = ident_White$sum,
     col=rgb(1,0,0,0.2), add=TRUE)
legend('topright', c('BIPOC students', 'White students'),
       fill=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)))
    
