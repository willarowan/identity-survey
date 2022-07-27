install.packages("stringr")
library("stringr")
library(ggplot2)

#dataframe with groups of BIPOC/White, WNB/Male
ident_M_BIPOC$racegen <- c("BIPOC, Male")
ident_WNB_BIPOC$racegen <- c("BIPOC, Women and Non-Binary")
ident_M_White$racegen <- c("White, Male")
ident_WNB_White$racegen <- c("White, Women and Non-Binary")
ident_racegen <- rbind(ident_M_White,ident_WNB_White, ident_M_BIPOC,ident_WNB_BIPOC)


#quick boxplot 
ggplot(ident_racegen, aes(x=racegen, y=sum_ident)) +
  geom_boxplot()+
  ggtitle("Geoscience Identity of Senior Geoscience Majors")+
  theme(axis.text.x = element_text(size=10, vjust = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  theme(axis.text.y = element_text(size=8))+
  ylab("Strength of Geoscience Identity")+
  theme(axis.title.y = element_text(size=14))+
  xlab("")

