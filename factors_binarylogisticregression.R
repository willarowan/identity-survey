install.packages("caret")
library(caret)

#creating test copy of factor data for building regression model
fact_race.blrm <- fact_race

#checking variables
dim(fact_race.blrm)
table(fact_race.blrm$race)

#creating dummy variables using as.factor()
fact_race.blrm$race <- as.factor(fact_race.blrm$race)

#ignore NAs but keep complete dataset
#fact_race.blrm <- na.omit(fact_race.blrm)

#check target variable with quick vis
ggplot(fact_race.blrm, aes(fact_13)) +
  geom_histogram(aes(fill = race), color = 'black', binwidth = 1)
