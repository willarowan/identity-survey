library(readr)

adult <- read_csv("./binaryregression_exampledataset.data")
# Checking the structure of adult data
str(adult)

# Subsetting the data and keeping the required variables
names(adult)[1] = 'Age'
names(adult)[2] = 'Workclass'
names(adult)[6] = 'Maritalstatus'
names(adult)[15] = 'Class'
adult <- adult[ ,c("Workclass", "Maritalstatus", "Age", "Class")]
# Checking the dim
dim(adult)

# Generating the frequency table
table(adult$Workclass)
# Generating the frequency table
table(adult$Maritalstatus)

# Converting to factor variables
adult$Workclass <- as.factor(adult$Workclass)
adult$Maritalstatus <- as.factor(adult$Maritalstatus)
adult$Class <- as.factor(adult$Class)

# Converting ? to NA
adult[adult == "?"] <- NA

# Keeping only the na.omit() function
adult <- na.omit(adult)

library(ggplot2)
ggplot(adult, aes(Age)) +
  geom_histogram(aes(fill = Class), color = "black", binwidth = 2)

# Loading caret library
require(caret)
# Splitting the data into train and test
index <- createDataPartition(adult$Class, p = .70, list = FALSE)
train <- adult[index, ]
test <- adult[-index, ]

# Training the model
logistic_model <- glm(Class ~ ., family = binomial(), train)

# Checking the model
summary(logistic_model)

# Predicting in the test dataset
pred_prob <- predict(logistic_model, test, type = "response")

# Converting from probability to actual output
train$pred_class <- ifelse(logistic_model$fitted.values >= 0.5, ">50K", "<=50K")

# Generating the classification table
ctab_train <- table(train$Class, train$pred_class)
ctab_train