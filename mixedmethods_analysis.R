install.packages('tidyverse')
install.packages('tidyr')
library(tidyverse)
library(tidyr)

###Start chunk to run
library(tidyverse)

surveycodedresults <- read.csv('https://raw.githubusercontent.com/willarowan/identity-survey/main/survey-exports/Survey_coded_withidentsum.csv')
surveycoded <- surveycodedresults

#qual.HispLat <- subset(surveycoded, DG_What.is.your.ethnicity...Hispanic.or.Latino == '1')

#Let's make groups of themes
supp.rel <- subset(surveycoded, Family.commitments=='1'& Sentiment..Positive=='1'|
                        Family.support=='1'& Sentiment..Positive=='1'|
                        Mentors=='1'|
                        Respect.from.professors=='1'& Sentiment..Positive=='1'|
                        Peer.support=='1'|Role.models=='1')
microagg <- subset(surveycoded, Encounters.with.ageism=='1'|
                        Encounters.with.homophobia=='1'|
                        Encounters.with.sexism=='1'|Encounters.with.racism=='1')
excl.cult.soc <- subset(surveycoded, Department.culture=='1'& Sentiment..Negative=='1'|
                        Geoscience.culture=='1'& Sentiment..Negative=='1'|
                        Sense.of.belonging=='1'& Sentiment..Negative=='1'|
                        Cultural.differences=='1'|
                        Ethnic.cultural.values.and.socialization=='1'|
                        Encounters.with.ageism=='1'|
                        Encounters.with.homophobia=='1'|
                        Encounters.with.sexism=='1'|Encounters.with.racism=='1')
belong.comm <- subset(surveycoded, Department.culture=='1'& Sentiment..Positive=='1'|
                        Geoscience.culture=='1'& Sentiment..Positive=='1'|
                        Sense.of.belonging=='1'& Sentiment..Positive=='1')
int.aff <- subset(surveycoded, Interest=='1'& Sentiment..Positive=='1'|
                       Geoscience.self.efficacy=='1'& Sentiment..Positive=='1'|
                       Math.self.efficacy=='1'& Sentiment..Positive=='1'|
                       Connection.love.of.nature=='1'|Helping.others=='1'|
                       Protecting.the.environment=='1')
inadequacy <- subset(surveycoded, Burnout=='1'|
                       Imposter.syndrome=='1'|Mental.health=='1'|
                       Geoscience.self.efficacy=='1'& Sentiment..Negative=='1'|
                       Math.self.efficacy=='1'& Sentiment..Negative=='1'|
                       Personal.characteristics=='1')
career <- subset(surveycoded, Career.development.activities=='1'& Sentiment..Positive=='1'|
                   Geoscience.internships=='1'& Sentiment..Positive=='1'|
                   Geoscience.job.market=='1'& Sentiment..Positive=='1'|
                   Knowledge.of.geoscience.careers=='1'& Sentiment..Positive=='1'|
                   Salary=='1'& Sentiment..Positive=='1'|
                   Outdoors.work=='1'& Sentiment..Positive=='1')
career.lack <- subset(surveycoded, Career.development.activities=='1'& Sentiment..Negative=='1'|
                   Geoscience.internships=='1'& Sentiment..Negative=='1'|
                   Geoscience.job.market=='1'& Sentiment..Negative=='1'|
                   Knowledge.of.geoscience.careers=='1'& Sentiment..Negative=='1'|
                   Salary=='1'& Sentiment..Negative=='1'|
                   Outdoors.work=='1'& Sentiment..Negative=='1')
eng.classroom <- subset(surveycoded, Engaging.geoscience.course.content=='1'& Sentiment..Positive=='1'|
                      Course.selection=='1'& Sentiment..Positive=='1'|
                      Introductory.Geoscience=='1'& Sentiment..Positive=='1'|
                      Required.STEM.courses=='1'& Sentiment..Positive=='1'|Effective.instruction=='1'& Sentiment..Positive=='1'|
                      Field.experiences=='1'& Sentiment..Positive=='1')
othering <- subset(surveycoded, Indigenous.knowledge=='1'|Effective.instruction=='1'& Sentiment..Negative=='1'|
                     Field.experiences=='1'& Sentiment..Negative=='1')
extracurricular <- subset(surveycoded, Extracurricular.activities=='1'|
                    Lab.reading.groups=='1'|Teaching.experiences=='1'|
                    Research.experiences=='1'& Sentiment..Positive=='1'|Outdoor.experiences=='1'& Sentiment..Positive=='1'|
                    Travel=='1')
issues.course <- subset(surveycoded, Required.STEM.courses=='1'& Sentiment..Negative=='1'|
                          Required.geoscience.courses=='1'& Sentiment..Negative=='1'|
                          Awareness.of.geoscience=='1'& Sentiment..Negative=='1'|
                          Department.academics=='1'&Sentiment..Negative=='1')
str.barr <- subset(surveycoded, Covid.19.impacts=='1'|Remote.modality=='1'|
                          Representation.in.faculty.and.staff=='1'& Sentiment..Negative=='1'|
                          Fiscal.abilities=='1'|Department.funding=='1'& Sentiment..Negative=='1'|
                          Physical.accessibility=='1'& Sentiment..Negative=='1')

#Adding a column with name of theme
supp.rel$theme <- c("supp.rel")
microagg$theme <- c("microagg")
excl.cult.soc$theme <- c("excl.cult.soc")
belong.comm$theme <- c("belong.comm")
int.aff$theme <- c("int.aff")
inadequacy$theme <- c("inadequacy")
career$theme <- c("career")
career.lack$theme <- c("career.lack")
eng.classroom$theme <- c("eng.classroom")
othering$theme <- c("othering")
extracurricular$theme <- c("extracurricular")
issues.course$theme <- c("issues.course")
str.barr$theme <- c("str.barr")
surveycoded.themes <- rbind(supp.rel,microagg,excl.cult.soc,belong.comm,
                            int.aff,inadequacy,career,career.lack,
                            eng.classroom,othering,
                            extracurricular,issues.course,str.barr)
retention.themes<- rbind(supp.rel,belong.comm,int.aff,career,eng.classroom,
                         extracurricular)
exclusion.themes<- rbind(microagg,excl.cult.soc,inadequacy,career.lack,
                         othering,issues.course,str.barr)

#any difference in identity score? No
t.test(x=retention.themes$geoidentity_sum,
       y=exclusion.themes$geoidentity_sum)

### End chunk to run

surveycoded.themes %>%
  group_by(theme) %>%
  summarise_at(vars(geoidentity_sum), list(name = mean))


#Who considered leaving the major?
leaving <- subset(surveycoded, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.microagg <- subset(microagg, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.excl.cult.soc <- subset(excl.cult.soc, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.inadequacy <- subset(inadequacy, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.career.lack <- subset(career.lack, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.othering <- subset(othering, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.issues.course <- subset(issues.course, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
leaving.str.barr <- subset(str.barr, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')

#do they have lower identity scores than the whole group?
t.test(x = ident$sum_ident,
       y = leaving$geoidentity_sum) #a little bit lower, not significant

#who's answering?
supp.rel%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value == 'yes') %>% print(n = 19)

supp.rel.white <- subset(supp.rel, 
                  DG_What.is.your.ethnicity...Non.Hispanic.or.Latino =='yes' &
                  DG_What.is.your.racial.background...White =='yes')

??gather
#what are their race/ethnicity identities?
leaving %>%
  gather(x, value) %>%
  group_by(x)%>%
  tally(value == 'yes')%>%
  print(n=20)
