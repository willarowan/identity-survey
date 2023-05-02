

surveycodedresults <- read.csv('https://raw.githubusercontent.com/willarowan/identity-survey/main/Survey_coded.csv')
surveycoded <- surveycodedresults

#Who considered leaving the major?
leaving <- subset(surveycoded, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
#who considered leaving because of adverse group dynamics?
leaving_group <- subset(leaving, Department.culture=='1'|Geoscience.culture=='1'|
                          Sense.of.belonging=='1'|Cultural.differences=='1'|
                          Ethnic.cultural.values.and.socialization=='1'|
                          Encounters.with.ageism=='1'|
                          Encounters.with.homophobia=='1'|
                          Encounters.with.sexism=='1'|Encounters.with.racism=='1')

#who in general experienced adverse group dynamics?
adverse_group <- subset(surveycoded, Sentiment..Negative=='1' &
                          Department.culture=='1'|Geoscience.culture=='1'|
                          Sense.of.belonging=='1'|Cultural.differences=='1'|
                          Ethnic.cultural.values.and.socialization=='1'|
                          Encounters.with.ageism=='1'|
                          Encounters.with.homophobia=='1'|
                          Encounters.with.sexism=='1'|Encounters.with.racism=='1')