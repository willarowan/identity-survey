install.packages(dplyr)
library(dplyr)
data <- read.csv(file = 'https://raw.githubusercontent.com/willarowan/identity-survey/main/Survey_7_7_22.csv')
data <- as.data.frame(data[-c(1,2),-c(1:13)]) #get rid of unwanted rows/columns

# test cleanup on a subset of data
test <- data[1:15,] #using small subset
#cut out unnecessary columns and rows
test <- as.data.frame(test[-c(1,2),-c(1:13)])
#rename columns
names(test) <- c(paste0("ident_",1:20),paste0("fact_",1:15),
                 paste0("shrtansw_",1:5), paste0("demo_",1:6)) 
# Rename identity variables
test[test == '5 = Strongly Agree'] <- 5
test[test == '4 = Agree'] <- 4
test[test == '3 = Neither Agree nor Disagree'] <- 3
test[test == '2 = Disagree'] <- 2
test[test == '1 = Strongly Disagree'] <- 1
# Rename factors variables
test[test == '2 = Had a strong positive impact'] <- 2
test[test == '1 = Had a positive impact'] <- 1
test[test == '0 = Had neither a positive nor a negative impact']<- 0
test[test == '-1 = Had a negative impact'] <- -1
test[test == '-2 = Had a strong negative impact'] <- -2
test[test == 'N/A = Not applicable/ Did not experience this'] <- NA

str(test)
