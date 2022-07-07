# Fix reverse-coded questions
test$ident_7[which(test$ident_7 == 5)] <- 1
test$ident_7[which(test$ident_7 == 4)] <- 2
test$ident_7[which(test$ident_7 == 2)] <- 4 # but then this changes some back
test$ident_7[which(test$ident_7 == 1)] <- 5

#For now let's look at data without reverse-coded questions

data2 <- data #make copy to mess around with
data2 <- as.data.frame(data2[-c(1,2),-c(1:13)]) #get rid of unwanted rows/columns

#rename columns
names(data2) <- c(paste0("ident_",1:20),paste0("fact_",1:15),
                 paste0("shrtansw_",1:5), paste0("demo_",1:6)) 

# Rename identity variables
data2[data2 == '5 = Strongly Agree'] <- 5
data2[data2 == '4 = Agree'] <- 4
data2[data2 == '3 = Neither Agree nor Disagree'] <- 3
data2[data2 == '2 = Disagree'] <- 2
data2[data2 == '1 = Strongly Disagree'] <- 1

# Rename factors variables
data2[data2 == '2 = Had a strong positive impact'] <- 2
data2[data2 == '1 = Had a positive impact'] <- 1
data2[data2 == '0 = Had neither a positive nor a negative impact']<- 0
data2[data2 == '-1 = Had a negative impact'] <- -1
data2[data2 == '-2 = Had a strong negative impact'] <- -2
data2[data2 == 'N/A = Not applicable/ Did not experience this'] <- NA

#prep dataframe for stats
test_demo <- select(data2, contains("demo")) #just demo
test_ident <- select(data2, contains("ident")) #just identity scores
# test_ident <- test_ident[,grepl("^ident",names(test_ident))] #just identity score columns
test_ident <- data.frame(subset(test_ident, select = -c(ident_7,ident_9,ident_12,ident_13,ident_14,ident_18,ident_20))) #take out reverse-coded
test_ident <- apply(test_ident,2,as.numeric) #coerce from char to numeric
sum <- apply(test_ident,1,sum) #total identity score of each entry
test_ident <- data.frame(cbind(test_ident,sum)) #add sum scores as new column, coerce back to dataframe
# test_ident$sum <- apply(test_ident,1,sum) #alternative to cbind()

test_demo_ident <- cbind(test_ident,test_demo) #bind with demo
# tapply(test_demo_ident['sum'],test_demo_ident['demo_4'],mean) # didn't work - must have same length

#total identity score by racial identity
aggregate(formula = sum ~ demo_4,
          FUN = mean,
          data = test_demo_ident)
#alternate which also works
test_demo_ident %>% # using dplyr
  group_by(demo_4) %>%
  summarise(avg_sum = mean(sum, na.rm=TRUE))

#total identity score by gender identity
aggregate(formula = sum ~ demo_1,
          FUN = mean,
          data = test_demo_ident)

#checking for statistical significance
t.test(formula = sum ~ demo_1,
       data = test_demo_ident,
       subset = demo_1 %in% c('Female', 'Male'))

aov(formula = sum ~ demo_4,
               data = test_demo_ident)
