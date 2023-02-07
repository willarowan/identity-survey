install.packages("caret")
library(caret)


#creating test copy of factor data for building regression model
fact_race.blrm <- fact_race
fact_race.blrm1 <- fact_race.blrm[, c("fact_1","race")]

fact_racegen.blrm <- fact_racegen

#checking variables
dim(fact_race.blrm1)

#generating the frequency table
table(fact_race.blrm1$fact_1)

#converting to factor variables
fact_race.blrm1$fact_1 <- as.factor(fact_race.blrm1$fact_1)
fact_race.blrm1$race <- as.factor(fact_race.blrm1$race)

fact_racegen.blrm$racegen <- as.factor(fact_racegen.blrm$racegen)

#ignore NAs but keep complete dataset
#fact_race.blrm <- na.omit(fact_race.blrm)

#check a target variable with quick vis
ggplot(fact_race.blrm1, aes(fact_1)) +
  geom_histogram(aes(fill = race), color = 'black', binwidth = 1)

ggplot(fact_racegen.blrm, aes(fact_13)) +
  geom_histogram(aes(fill = racegen), color = 'black', binwidth = 1)

#building the model
require(caret)
#split data into train and test
#note to self - why p=.70? that's percentage of data that goes to training
#?createDataPartition
race_index.blrm1 <- createDataPartition(fact_race.blrm1$race, p=.70, list = FALSE)
race_train.blrm1 <- fact_race.blrm1[race_index.blrm1, ]
race_test.blrm1 <- fact_race.blrm1[-race_index.blrm1, ]

racegen_index.blrm <- createDataPartition(fact_racegen.blrm$racegen, 
                                          p=.70, list = FALSE)
racegen_train.blrm <- fact_racegen.blrm[racegen_index.blrm, ]
racegen_test.blrm <- fact_racegen.blrm[-racegen_index.blrm, ]

#check the variables again
dim(race_train.blrm1)

#how many unique values for each variable?
sapply(lapply(race_train.blrm1, unique), length)
#what are the unique values?
lapply(race_train.blrm1[c('race', 'fact_1')], unique)

?glm()
#training the model for one of the factors
logistic_model_race.blrm1 <- glm(race ~ ., family = binomial, race_train.blrm1)
summary(logistic_model_race.blrm1)

logistic_model_racegen13.blrm <- glm(racegen ~ fact_13, family = binomial, 
                             racegen_train.blrm)
summary(logistic_model_racegen13.blrm)

#a different factor
logistic_model12.blrm <- glm(race ~ fact_12, family = binomial, train.blrm)
summary(logistic_model12.blrm)

# Predicting in the test dataset
pred_prob <- predict(logistic_model_race.blrm1, test, type = "response")
