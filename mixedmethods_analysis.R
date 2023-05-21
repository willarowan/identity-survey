surveycodedresults <- read.csv('https://raw.githubusercontent.com/willarowan/identity-survey/main/survey-exports/Survey_coded_withidentsum.csv')
surveycoded <- surveycodedresults

qual.HispLat <- subset(surveycoded, DG_What.is.your.ethnicity...Hispanic.or.Latino == '1')

#Let's make groups of themes
interpersonal <- subset(surveycoded, Family.commitments=='1'|Family.support=='1'|
                        Mentors=='1'|Respect.from.professors=='1'|
                        Peer.support=='1'|Role.models=='1'|
                        Encounters.with.ageism=='1'|
                        Encounters.with.homophobia=='1'|
                        Encounters.with.sexism=='1'|Encounters.with.racism=='1')
cultural.social <- subset(surveycoded, Department.culture=='1'|
                        Geoscience.culture=='1'|Sense.of.belonging=='1'|
                        Cultural.differences=='1'|
                        Ethnic.cultural.values.and.socialization=='1'|
                        Encounters.with.ageism=='1'|
                        Encounters.with.homophobia=='1'|
                        Encounters.with.sexism=='1'|Encounters.with.racism=='1')
individual <- subset(surveycoded, Interest=='1'|Burnout=='1'|
                       Imposter.syndrome=='1'|Mental.health=='1'|
                       Geoscience.self.efficacy=='1'|
                       Math.self.efficacy=='1'|Personal.characteristics=='1'|
                       Connection.love.of.nature=='1'|Helping.others=='1'|
                       Protecting.the.environment=='1')
career <- subset(surveycoded, Career.development.activities=='1'|
                   Geoscience.internships=='1'|Geoscience.job.market=='1'|
                   Knowledge.of.geoscience.careers=='1'|Salary=='1'|
                   Outdoors.work=='1')
classroom <- subset(surveycoded, Engaging.geoscience.course.content=='1'|
                      Indigenous.knowledge=='1'|Course.selection=='1'|
                      Introductory.Geoscience=='1'|
                      Required.STEM.courses=='1'|Effective.instruction=='1'|
                      Field.experiences=='1')
extracurricular <- subset(surveycoded, Awareness.of.geoscience=='1'|
                    Department.academics=='1'|Extracurricular.activities=='1'|
                    Lab.reading.groups=='1'|Teaching.experiences=='1'|
                    Research.experiences=='1'|Outdoor.experiences=='1'|
                    Travel=='1')
institutional <- subset(surveycoded, Covid.19.impacts=='1'|Remote.modality=='1'|
                          Representation.in.faculty.and.staff=='1'|
                          Fiscal.abilities=='1'|Department.funding=='1'|
                          Physical.accessibility=='1'|
                          Encounters.with.ageism=='1'|
                          Encounters.with.homophobia=='1'|
                          Encounters.with.sexism=='1'|Encounters.with.racism=='1')

#Positive vs. negative experiences for each theme
interpersonal.neg <- subset(interpersonal, Sentiment..Negative=='1')
interpersonal.pos <- subset(interpersonal, Sentiment..Positive=='1')
cultural.social.neg <- subset(cultural.social, Sentiment..Negative=='1')
cultural.social.pos <- subset(cultural.social, Sentiment..Positive=='1')
individual.neg <- subset(individual, Sentiment..Negative=='1')
individual.pos <- subset(individual, Sentiment..Positive=='1')
career.neg <- subset(career, Sentiment..Negative=='1')
career.pos <- subset(career, Sentiment..Positive=='1')
classroom.neg <- subset(classroom, Sentiment..Negative=='1')
classroom.pos <- subset(classroom, Sentiment..Positive=='1')
extracurricular.neg <- subset(extracurricular, Sentiment..Negative=='1')
extracurricular.pos <- subset(extracurricular, Sentiment..Positive=='1')
institutional.neg <- subset(institutional, Sentiment..Negative=='1')
institutional.pos <- subset(institutional, Sentiment..Positive=='1')

#Adding a column with name of theme
interpersonal.neg$theme <- c("interpersonal.neg")
interpersonal.pos$theme <- c("interpersonal.pos")
cultural.social.neg$theme <- c("cultural.social.neg")
cultural.social.pos$theme <- c("cultural.social.pos")
individual.neg$theme <- c("individual.neg")
individual.pos$theme <- c("individual.pos")
career.neg$theme <- c("career.neg")
career.pos$theme <- c("career.pos")
classroom.neg$theme <- c("classroom.neg")
classroom.pos$theme <- c("classroom.pos")
extracurricular.neg$theme <- c("extracurricular.neg")
extracurricular.pos$theme <- c("extracurricular.pos")
institutional.neg$theme <- c("institutional.neg")
institutional.pos$theme <- c("institutional.pos")
surveycoded.themes <- rbind(interpersonal.neg,interpersonal.pos,
                            cultural.social.neg,cultural.social.pos,
                            individual.neg,individual.pos,
                            career.neg,career.pos,classroom.neg,classroom.pos,
                            extracurricular.neg,extracurricular.pos,
                            institutional.neg,institutional.pos)
surveycoded.themes %>%
  group_by(theme) %>%
  summarise_at(vars(geoidentity_sum), list(name = mean))


#Who considered leaving the major?
leaving <- subset(surveycoded, Did.you.ever.consider.leaving.your.geoscience.major..If.so..why.=='1'
                  & Sentiment..Negative=='1')
#do they have lower identity scores than the whole group?
t.test(x = ident$sum_ident,
       y = leaving$geoidentity_sum) #a little bit lower, not significant


