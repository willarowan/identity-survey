install.packages('car')
library(car)
library(dplyr)


# ------- Start chunk to run -----------

#Sum identity score
ident <- subset(survey_rev,select=c(ident_1:ident_9,ident_15:ident_20)) #just identity score columns
demo <- survey_rev[,grepl("^demo",names(survey_rev))] #just demo info
ident <- apply(ident,2,as.numeric) #coerce from char to numeric
sum_ident <- apply(ident,1,sum) #total identity score of each entry
ident <- data.frame(cbind(ident,sum_ident)) #add sum scores as new column, coerce back to dataframe
ident <- as.data.frame(cbind(ident,demo)) #bind with demo, coerce to dataframe

#Skip below chunk until it's fixed

#can the above be turned into a function?
# sum.ident.score <- function(data){
#   ident <- data[,grepl("^ident",names(data))]
#   demo <- data[,grepl("^demo",names(data))] #just demo info
#   ident <- apply(ident,2,as.numeric) #coerce from char to numeric
#   sum <- apply(ident,1,sum) #total identity score of each entry
#   ident <- data.frame(cbind(ident,sum)) #add sum scores as new column, coerce back to dataframe
#   ident <- as.data.frame(cbind(ident,demo))
#   return(ident)
# }
# ident<- sum.ident.score(survey_rev) # that works

#Above funtion, but customizable for anything I want to index in the survey?
#(B/c above fuction just pulls full identity score)

#Working code for dataframe with summed interest section scores and demo info
#I want to make this a function, and be able to change which columns I subset
# ident_int <- subset(survey_rev,select=c(ident_10:ident_14)) #interest section
# demo <- survey_rev[,grepl("^demo",names(survey_rev))]
# ident_int <- apply(ident_int,2,as.numeric) #coerce from char to numeric
# sum_int <- apply(ident_int,1,sum) #total identity score of each entry
# ident_int <- data.frame(cbind(ident_int,sum_int)) #add sum scores as new column, coerce back to dataframe
# ident_int <- as.data.frame(cbind(ident_int,demo)) 
# 
# #trying as a function, argument 'index' for columns to subset
# sum.score <- function(data, index){
#   ident <- subset(data,select=c(index))
#   demo <- data[,grepl("^demo",names(data))]
#   ident <- apply(ident,2,as.numeric) #coerce from char to numeric
#   sum <- apply(ident,1,sum) #total identity score of each entry
#   ident <- data.frame(cbind(ident,sum)) #add sum scores as new column, coerce back to dataframe
#   ident <- as.data.frame(cbind(ident,demo))
#   return(ident)
# }
# #trying function with specifying interest section columns
# ident_int <- sum.score(survey_rev, survey_rev[,ident_10:ident_14]) #doesn't work


#test dataframe with binning into BIPOC
?grep
ident_HispLat <- subset(ident, demo_3=="Hispanic or Latino") 
ident_BIPOC <- ident[grep("Black|Asian|Other|Native|More|I'll", ident$demo_4), ]
ident_BIPOC <- rbind(ident_HispLat,ident_BIPOC)
ident_White <- subset(ident, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#sub-divide White and BIPOC into gender
ident_WNB_BIPOC <- ident_BIPOC[grep("Female|Non-Binary|Intersex|I'll", ident_BIPOC$demo_1), ]
ident_WNB_White <- ident_White[grep("Female|Non-Binary|Intersex|I'll", ident_White$demo_1), ]
ident_M_White <- ident_White[grep("Male", ident_White$demo_1), ]
ident_M_BIPOC <- ident_BIPOC[grep("Male", ident_BIPOC$demo_1), ]
#subset into seperate Women & Non-Binary? *Very few entries with non-binary
#ident_W_BIPOC <- ident_BIPOC[grep("Female", ident_BIPOC$demo_1), ]
#ident_NB_BIPOC <- ident_BIPOC[grep("Non-Binary|Intersex|I'll", ident_BIPOC$demo_1), ]
#ident_W_White <- ident_White[grep("Female", ident_White$demo_1), ]
#ident_NB_White <- ident_White[grep("Non-Binary|Intersex|I'll", ident_White$demo_1), ]

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
ident_M_BIPOC$racegen <- c("BIPOC, Male")
ident_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
ident_M_White$racegen <- c("White, Male")
ident_WNB_White$racegen <- c("White, Women and Non-Binary")
ident_racegen <- rbind(ident_M_White,ident_WNB_White, ident_M_BIPOC,ident_WNB_BIPOC)

#Add column with race identifiers
ident_BIPOC$race <- c("BIPOC")
ident_White$race <- c("White")
ident_race <- rbind(ident_BIPOC,ident_White)

#binning into Just gender identity
ident_M <- subset(ident, demo_1=='Male')
ident_WNB <- ident[grep("Female|Non-Binary|Intersex", ident$demo_1), ]


# ------ End chunk to run --------


#statistics time

#total identity score by racial identity - returns all combos
aggregate(formula = sum_ident ~ race,
          FUN = mean,
          data = ident_race)
#alternate which also works
ident_race %>% # using dplyr
  group_by(race) %>%
  summarise(avg_sum_ident = mean(sum_ident, na.rm=TRUE))

#total identity score by gender identity - returns all combos
aggregate(formula = sum ~ demo_1,
          FUN = mean,
          data = ident)

#checking for statistical significance with subsetting example
t.test(formula = sum ~ demo_1,
       data = ident,
       subset = demo_1 %in% c('Female', 'Male'))

aov(formula = sum ~ demo_4,
    data = ident)

#Evaluating major hypothesis of project
#difference in identity scores between White and BIPOC?
t.test(x = ident_BIPOC$sum,
       y = ident_White$sum)

cor.test(x = ident_BIPOC$sum,
         y = ident_White$sum) #must have the same length (doesn't work)


ident.cstest <- chisq.test(x = table(ident_BIPOC$sum,
                                     ident_White$sum)) #must have same length


#T-test bonanza

#Women & Non-binary (WNB) BIPOC and WNB White?
t.test(x = ident_WNB_BIPOC$sum,
       y = ident_WNB_White$sum) 

#WNB and Male White?
t.test(x = ident_M_White$sum,
       y = ident_WNB_White$sum)

#WNB and Male BIPOC?
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
t.test(x = ident_WNB$sum,
       y = ident_M$sum)


#linear regression and anova

#are variables of same length?
with(ident_racegen,
     table(sum_ident, racegen))

#race and gender
racegen.lm <- lm(sum_ident ~ racegen,
   data = ident_racegen)
summary(racegen.lm)

racegen.aov <- aov(racegen.lm)
summary(racegen.aov)

#just race
race.lm <- lm(sum_ident ~ race,
                 data = ident_race)
summary(race.lm)

race.aov <- aov(race.lm)
summary(race.aov)

#comparing models of race vs racegen with anova
anova(race.lm, racegen.lm) #error msg not same size dataset

ident.II.aov <- car::Anova(racegen.lm, type = 2)
summary(ident.II.aov)


#more complex statistics
#Kolmogorov-Smirnov test for normal distribution
ks.test(ident_BIPOC$sum_ident, 'pnorm')

ks.test(ident_White$sum_ident, 'pnorm')
?ks.test()    
#p-value is sufficiently small to indicate non-normal distribution
#warning msg 'ties should not be present'

#Mann-Whitney U test
#for two variables
wilcox.test(sum_ident ~ race,
            data = ident_race,
            exact = FALSE) #p-value = .007
?wilcox.test()

#Kruskal-Wallis test: non-parametric alt for one-way ANOVA
kruskal.test(sum_ident ~ racegen, data = ident_racegen)

pairwise.wilcox.test(ident_racegen$sum_ident, ident_racegen$racegen,
                     p.adjust.method = "BH")
