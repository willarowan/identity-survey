

surveycodedresults <- read.csv('https://raw.githubusercontent.com/willarowan/identity-survey/main/survey_coded_withidentsum.csv')
surveycoded <- surveycodedresults

qual.HispLat <- subset(surveycoded, DG_What.is.your.ethnicity...Hispanic.or.Latino == '1')

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

#Who experienced exclusionary social interactions?
adverse_soc.int <- subset(surveycoded, Sentiment..Negative=='1' &
                          Cultural.differences=='1'|Respect.from.professors=='1'|
                          Ethnic.cultural.values.and.socialization=='1'|
                          Effective.instruction=='1'|
                          Encounters.with.ageism=='1'|
                          Encounters.with.homophobia=='1'|
                          Encounters.with.sexism=='1'|Encounters.with.racism=='1')

#who in general experienced adverse outdoor experiences?
adverse_outdoor <- subset(surveycoded, Sentiment..Negative=='1' &
                          Outdoor.experiences=='1'|Field.experiences=='1'|
                          Travel=='1'|Outdoors.work=='1'|
                          Physical.accessibility=='1')