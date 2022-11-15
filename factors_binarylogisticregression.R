install.packages("caret")
library(caret)


#creating test copy of factor data for building regression model
fact_race.blrm <- fact_race
fact_racegen.blrm <- fact_racegen

#checking variables
dim(fact_race.blrm)
table(fact_race.blrm$race)

#creating dummy variables using as.factor()
fact_race.blrm$race <- as.factor(fact_race.blrm$race)
fact_racegen.blrm$racegen <- as.factor(fact_racegen.blrm$racegen)

#ignore NAs but keep complete dataset
#fact_race.blrm <- na.omit(fact_race.blrm)

#check a target variable with quick vis
ggplot(fact_race.blrm, aes(fact_13)) +
  geom_histogram(aes(fill = race), color = 'black', binwidth = 1)

ggplot(fact_racegen.blrm, aes(fact_13)) +
  geom_histogram(aes(fill = racegen), color = 'black', binwidth = 1)

#building the model
require(caret)
#split data into train and test
#note to self - why p=.70? that's percentage of data that goes to training
?createDataPartition
race_index.blrm <- createDataPartition(fact_race.blrm$race, p=.70, list = FALSE)
race_train.blrm <- fact_race.blrm[race_index.blrm, ]
race_test.blrm <- fact_race.blrm[-race_index.blrm, ]

racegen_index.blrm <- createDataPartition(fact_racegen.blrm$racegen, 
                                          p=.70, list = FALSE)
racegen_train.blrm <- fact_racegen.blrm[racegen_index.blrm, ]
racegen_test.blrm <- fact_racegen.blrm[-racegen_index.blrm, ]

#check the variables again
dim(race_train.blrm)
#how many unique values for each variable?
sapply(lapply(race_train.blrm, unique), length)
#what are the unique values?
lapply(race_train.blrm[c('race', 'fact_13')], unique)

#training the model for one of the factors
logistic_model_race13.blrm <- glm(race ~ fact_13, family = binomial, race_train.blrm)
summary(logistic_model_race13.blrm)

logistic_model_racegen13.blrm <- glm(racegen ~ fact_13, family = binomial, 
                             racegen_train.blrm)
summary(logistic_model_racegen13.blrm)

#a different factor
logistic_model12.blrm <- glm(race ~ fact_12, family = binomial, train.blrm)
summary(logistic_model12.blrm)
