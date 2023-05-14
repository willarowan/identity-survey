library(dplyr)
survey_mixedmethods <- read.csv("~/R/Thesis_R/Survey_complete_mixedmethods.csv")

#rename columns
names(survey_mixedmethods) <- c(paste0("ident_",1:20),paste0("fact_",1:15),
                   paste0("shrtansw_",1:5), paste0("demo_",1:6))

# Rename identity variables
survey_mixedmethods[survey_mixedmethods == '5 = Strongly Agree'] <- 5
survey_mixedmethods[survey_mixedmethods == '4 = Agree'] <- 4
survey_mixedmethods[survey_mixedmethods == '3 = Neither Agree nor Disagree'] <- 3
survey_mixedmethods[survey_mixedmethods == '2 = Disagree'] <- 2
survey_mixedmethods[survey_mixedmethods == '1 = Strongly Disagree'] <- 1
# Rename factors variables
survey_mixedmethods[survey_mixedmethods == '2 = Had a strong positive impact'] <- 2
survey_mixedmethods[survey_mixedmethods == '1 = Had a positive impact'] <- 1
survey_mixedmethods[survey_mixedmethods == '0 = Had neither a positive nor a negative impact']<- 0
survey_mixedmethods[survey_mixedmethods == '-1 = Had a negative impact'] <- -1
survey_mixedmethods[survey_mixedmethods == '-2 = Had a strong negative impact'] <- -2
survey_mixedmethods[survey_mixedmethods == 'N/A = Not applicable/ Did not experience this'] <- NA

# Fix reverse-coded questions
columnsToReverse <- c('ident_7','ident_9','ident_12','ident_13','ident_14','ident_18','ident_20')
survey_mixedmethods[,grepl("^ident",names(survey_mixedmethods))] <- apply(survey_mixedmethods,2,as.numeric, na.rm = TRUE)
warnings()
survey_mixedmethods[,columnsToReverse] <- 6-survey_mixedmethods[,columnsToReverse]
survey_mixedmethods <- as.data.frame(survey_mixedmethods)

#sum identity scores into a new column
ident_mixedmethods <- subset(survey_mixedmethods_rev,select=c(ident_1:ident_9,ident_15:ident_20))
ident_mixedmethods <- ident_mixedmethods %>%
  mutate(ident_sum = rowSums(across(c(ident_1:ident_20))))
survey_mixedmethods <- cbind(ident_mixedmethods$ident_sum, survey_mixedmethods)
