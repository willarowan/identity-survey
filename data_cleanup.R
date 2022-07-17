install.packages(dplyr)
library(dplyr)
results <- read.csv(file = 'https://raw.githubusercontent.com/willarowan/identity-survey/main/Survey_7_7_22.csv')
survey <- results #make a copy

#get rid of unwanted rows/columns, invalid responses
survey <- subset(survey, Q13=="Yes" & Q14=="Yes") #eligibility & consent qs
survey <- subset(survey,select=-c(1:13))
survey <- subset(survey,select=-c(47))
#rename columns
names(survey) <- c(paste0("ident_",1:20),paste0("fact_",1:15),
                   paste0("shrtansw_",1:5), paste0("demo_",1:6))

# Rename identity variables
survey[survey == '5 = Strongly Agree'] <- 5
survey[survey == '4 = Agree'] <- 4
survey[survey == '3 = Neither Agree nor Disagree'] <- 3
survey[survey == '2 = Disagree'] <- 2
survey[survey == '1 = Strongly Disagree'] <- 1
# Rename factors variables
survey[survey == '2 = Had a strong positive impact'] <- 2
survey[survey == '1 = Had a positive impact'] <- 1
survey[survey == '0 = Had neither a positive nor a negative impact']<- 0
survey[survey == '-1 = Had a negative impact'] <- -1
survey[survey == '-2 = Had a strong negative impact'] <- -2
survey[survey == 'N/A = Not applicable/ Did not experience this'] <- NA

str(survey)

# Fix reverse-coded questions
columnsToReverse <- c('ident_7','ident_9','ident_12','ident_13','ident_14','ident_18','ident_20')
survey_rev <- survey
survey_rev[,grepl("^ident",names(survey_rev))] <- apply(survey_rev,2,as.numeric, na.rm = TRUE)
warnings()
survey_rev[,columnsToReverse] <- 6-survey_rev[,columnsToReverse]
survey_rev <- as.data.frame(survey_rev)

# test cleanup on a subset of data
test <- results[1:15,] #using small subset
