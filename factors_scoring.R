library(dplyr)
library(WRS2)

#begin chunk to run

fact <- survey_rev[,grepl("^fact",names(survey_rev))] #just factors score columns
demo <- survey_rev[,grepl("^demo",names(survey_rev))] #just demo info
fact <- apply(fact,2,as.numeric, ra.rm=TRUE) #coerce from char to numeric
fact<- data.frame(fact) #coerce to dataframe
#add row with sum of factors ratings
#fact<- fact %>%
#  mutate(sum_fact=select(., fact_1:fact_14) %>% rowSums(na.rm=TRUE))
#fact %>%
#  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
fact <- as.data.frame(cbind(fact,demo)) #bind with demo, coerce to dataframe

#BIPOC and White subgroups
fact_HispLat <- subset(fact, demo_3=="Hispanic or Latino") 
fact_BIPOC <- fact[grep("Black|Asian|Other|Native|More|I'll", fact$demo_4), ]
fact_BIPOC <- rbind(fact_HispLat,fact_BIPOC)
fact_White <- subset(fact, demo_3=="Non-Hispanic or Latino" & demo_4=="White")

#subgroups for gender
fact_WNB_BIPOC <- fact_BIPOC[grep("Female|Non-Binary|Intersex|I'll", fact_BIPOC$demo_1), ]
fact_WNB_White <- fact_White[grep("Female|Non-Binary|Intersex|I'll", fact_White$demo_1), ]
fact_M_White <- fact_White[grep("Male", fact_White$demo_1), ]
fact_M_BIPOC <- fact_BIPOC[grep("Male", fact_BIPOC$demo_1), ]

#Add column wtih race & gender identifiers
#dataframe with groups of BIPOC/White, WNB/Male
fact_M_BIPOC$racegen <- c("BIPOC, Male")
fact_WNB_BIPOC$racegen <- c("BIPOC, Female and Non-Binary")
fact_M_White$racegen <- c("White, Male")
fact_WNB_White$racegen <- c("White, Female and Non-Binary")
fact_racegen <- rbind(fact_M_White,fact_WNB_White, fact_M_BIPOC,fact_WNB_BIPOC)

#Add column with race identifiers
fact_BIPOC$race <- c("BIPOC")
fact_White$race <- c("White")
fact_race <- rbind(fact_BIPOC,fact_White)

#end chunk to run

#how many of each rating for a given factor?
#fact_M_BIPOC %>% #doesn't count NAs
  #count(fact_1)
#make a new data frame to put counts into
#fact_M_BIPOC.counts <- data.frame(matrix(ncol = 15, nrow = 6))
#name columns to match
#names(fact_M_BIPOC.counts) <- c(paste0("fact_",1:14),paste0("racegen"))
#count ratings for each factor
#table(fact_M_BIPOC$fact_1, useNA = "ifany")


#are there significant differences?
#do bootstrapped anova for each one
fact11.boot.racegen <- t1waybt(fact_6~racegen,tr=.2,nboot=4999, data=fact_racegen)
#hist(fact1.boot.racegen$test)

#pairwise post-hoc tests
fact11.bootpairwise.racegen <- mcppb20(fact_14~racegen,tr=.2,nboot=4999, data=fact_racegen)

#just race?
t1waybt(fact_11~race,tr=.2,nboot=4999, data=fact_race)

#checking which group is higher when there is a small p-value
t.test(x=fact_WNB_White$fact_12,
       y=fact_M_BIPOC$fact_12)

#What are standard deviations of each factor?
lapply(fact_racegen[, 1:14], sd)

#Mann-Whitney U test
#for two variables
wilcox.test(fact_11 ~ race,
            data = fact_race,
            exact = FALSE) 

wilcox.test(x = fact_M_BIPOC$fact_13, y = fact_M_White$fact_13, 
            alternative = 'two.sided')

#Kruskal-Wallis test: non-parametric alt for one-way ANOVA
kruskal.test(fact_13 ~ racegen, data = fact_racegen)

pairwise.wilcox.test(fact_racegen$fact_10, fact_racegen$racegen,
                     p.adjust.method = "BH")


#Example

#microaggressions by race & gen?
pairwise.wilcox.test(fact_racegen$fact_13, fact_racegen$racegen,
                     p.adjust.method = "BH")

#microaggressions, comparing each race & gen combo one by one
wilcox.test(x = fact_WNB_BIPOC$fact_13, y = fact_M_White$fact_13, 
            alternative = 'two.sided')

wilcox.test(x = fact_WNB_White$fact_13, y = fact_M_White$fact_13, 
            alternative = 'two.sided')


#faculty mentors
t.test(x = fact_BIPOC$fact_1,
       y = fact_White$fact_1)

t.test(x = fact_WNB_BIPOC$fact_1,
       y = fact_M_White$fact_1)

t.test(x = fact_M_BIPOC$fact_1,
       y = fact_M_White$fact_1)

t.test(x = fact_WNB_White$fact_1,
       y = fact_M_White$fact_1)

t.test(x = fact_WNB_White$fact_1,
       y = fact_WNB_BIPOC$fact_1)

#research in class?
t.test(x = fact_BIPOC$fact_3,
       y = fact_White$fact_3)

t.test(x = fact_WNB_BIPOC$fact_3,
       y = fact_M_White$fact_3)

t.test(x = fact_M_BIPOC$fact_3,
       y = fact_M_White$fact_3)

t.test(x = fact_WNB_White$fact_3,
       y = fact_M_White$fact_3)

t.test(x = fact_WNB_White$fact_3,
       y = fact_WNB_BIPOC$fact_3)

#research outside class?
t.test(x = fact_BIPOC$fact_4,
       y = fact_White$fact_4)

t.test(x = fact_WNB_BIPOC$fact_4,
       y = fact_M_White$fact_4)

t.test(x = fact_M_BIPOC$fact_4,
       y = fact_M_White$fact_4)

t.test(x = fact_WNB_BIPOC$fact_4,
       y = fact_WNB_White$fact_4)

t.test(x = fact_WNB_White$fact_4,
       y = fact_M_White$fact_4)

t.test(x = fact_WNB_White$fact_4,
       y = fact_M_BIPOC$fact_4)

t.test(x = fact_WNB_BIPOC$fact_4,
       y = fact_M_BIPOC$fact_4)

#culural relevance/Indigenous knowledge
t.test(x = fact_BIPOC$fact_6,
       y = fact_White$fact_6)

t.test(x = fact_WNB_BIPOC$fact_6,
       y = fact_M_White$fact_6)

t.test(x = fact_M_BIPOC$fact_6,
       y = fact_M_White$fact_6)

t.test(x = fact_WNB_White$fact_6,
       y = fact_WNB_BIPOC$fact_6)

t.test(x = fact_WNB_White$fact_6,
       y = fact_M_BIPOC$fact_6)

#field experiences?
t.test(x = fact_BIPOC$fact_7,
       y = fact_White$fact_7)

t.test(x = fact_WNB_BIPOC$fact_7,
       y = fact_M_White$fact_7)

t.test(x = fact_M_BIPOC$fact_7,
       y = fact_M_White$fact_7)

t.test(x = fact_WNB_White$fact_7,
       y = fact_WNB_BIPOC$fact_7)

#knowledge of careers?
t.test(x = fact_BIPOC$fact_8,
       y = fact_White$fact_8)

t.test(x = fact_WNB_BIPOC$fact_8,
       y = fact_M_White$fact_8)

t.test(x = fact_M_BIPOC$fact_8,
       y = fact_M_White$fact_8)

t.test(x = fact_WNB_White$fact_8,
       y = fact_WNB_BIPOC$fact_8)

t.test(x = fact_WNB_White$fact_8,
       y = fact_M_BIPOC$fact_8)

#support from fam?
t.test(x = fact_BIPOC$fact_9,
       y = fact_White$fact_9)

t.test(x = fact_M_White$fact_9,
       y = fact_WNB_White$fact_9)

t.test(x = fact_WNB_BIPOC$fact_9,
       y = fact_M_BIPOC$fact_9)

t.test(x = fact_M_BIPOC$fact_9,
       y = fact_M_White$fact_9)

t.test(x = fact_WNB_BIPOC$fact_9,
       y = fact_M_White$fact_9)

t.test(x = fact_WNB_BIPOC$fact_9,
       y = fact_WNB_White$fact_9)

t.test(x = fact_M_BIPOC$fact_9,
       y = fact_WNB_White$fact_9)

#feeling a sense of belonging?
t.test(x = fact_BIPOC$fact_10,
       y = fact_White$fact_10) #significant

t.test(x = fact_M_White$fact_10,
       y = fact_WNB_White$fact_10)

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

#connections to Earth
t.test(x = fact_BIPOC$fact_11,
       y = fact_White$fact_11)

t.test(x = fact_M_White$fact_11,
       y = fact_WNB_White$fact_11)

t.test(x = fact_WNB_BIPOC$fact_11,
       y = fact_M_BIPOC$fact_11)

t.test(x = fact_M_BIPOC$fact_11,
       y = fact_M_White$fact_11)

t.test(x = fact_WNB_BIPOC$fact_11,
       y = fact_M_White$fact_11) #significant

t.test(x = fact_WNB_BIPOC$fact_11,
       y = fact_WNB_White$fact_11)

t.test(x = fact_M_BIPOC$fact_11,
       y = fact_WNB_White$fact_11)

#seeing oneself represented?
t.test(x = fact_BIPOC$fact_12,
       y = fact_White$fact_12)

t.test(x = fact_WNB_BIPOC$fact_12,
       y = fact_M_White$fact_12) #significant

t.test(x = fact_WNB_White$fact_12,
       y = fact_M_White$fact_12) #significant

t.test(x = fact_WNB_BIPOC$fact_12,
       y = fact_M_BIPOC$fact_12)

t.test(x = fact_M_BIPOC$fact_12,
       y = fact_M_White$fact_12)

#microaggressions?
t.test(x = fact_BIPOC$fact_13,
       y = fact_White$fact_13)

t.test(x = fact_WNB_BIPOC$fact_13,
       y = fact_M_White$fact_13) #significant

t.test(x = fact_WNB_White$fact_13,
       y = fact_M_White$fact_13) #significant

t.test(x = fact_WNB_BIPOC$fact_13,
       y = fact_M_BIPOC$fact_13)

t.test(x = fact_M_BIPOC$fact_13,
       y = fact_M_White$fact_13)

#macroagressions?
t.test(x = fact_BIPOC$fact_14,
       y = fact_White$fact_14)

t.test(x = fact_WNB_BIPOC$fact_14,
       y = fact_M_White$fact_14)

t.test(x = fact_WNB_White$fact_14,
       y = fact_M_White$fact_14) #significant

t.test(x = fact_WNB_BIPOC$fact_14,
       y = fact_M_BIPOC$fact_14)

t.test(x = fact_M_BIPOC$fact_14,
       y = fact_M_White$fact_14)