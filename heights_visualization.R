library(dslabs)
data(heights)
library(dplyr)
library(tidyverse)
library(gridExtra)
head(heights)
str(heights)
tab<-table(heights$sex)
tab
prop.table(tab)

heights%>% group_by(sex) %>% summarize(average=mean(height),standard_deviation=sd(height),minimum=min(height),median=median(height),maximum=max(height))

Male<- heights %>% filter(heights$sex=="Male")
Female<- heights %>% filter(heights$sex=="Female")

#Box-Plot
Box_plot<-heights%>%ggplot(aes(sex,height))+
  geom_boxplot(aes(fill=sex),color="black")+
  xlab("sex")+
  ylab("height")+
  ggtitle("BoxPlot")
Box_plot
#The plot immediately reveals that males are, on average, taller than females. 

# Histogram of Female heights
Hist_female<- Female%>% 
  ggplot(aes(height))+
  geom_histogram(binwidth=1,fill="pink",col="black")+
  xlab("Female heights in inches")+
  ggtitle("Histogram")
Hist_female

# Histogram of Male heights
Hist_male<- Male %>% 
  ggplot(aes(height))+
  geom_histogram(binwidth=1,fill="blue",col="black")+
  xlab("Male heights in inches")+
  ggtitle("Histogram")
Hist_male

#Smooth Density for Female
smoothdens_female<- Female %>%
  ggplot(aes(height))+geom_density(fill="pink")+
  xlab("Female heights in inches")+
  ggtitle("Smooth Density")
smoothdens_female

##Smooth Density for Male
smoothdens_male<-Male %>%
  ggplot(aes(height))+geom_density(fill="blue")+
  xlab("Male heights in inches")+
  ggtitle("Smooth Density")
smoothdens_male

#Smooth Density for Male & Female
Smooth_dens<- heights %>% ggplot(aes(height,fill=sex))+
  geom_density(alpha=0.2)+
  xlab("Heights in inches")+
  ggtitle("Smooth Density")
Smooth_dens

#QQ Plot for Female
QQ_Female<- Female %>%
    ggplot(aes(sample=scale(height)))+
    geom_qq()+
    geom_abline(lty=2,color="dark grey")+
    xlab("Female height")+
    ylab("height")+
    ggtitle("QQ Plot")
QQ_Female

Female  %>% 
  top_n(5,desc(height))%>%
  pull(height)

##QQ Plot for Male
QQ_Male<- Male %>%
  ggplot(aes(sample=scale(height)))+
  geom_qq()+
  geom_abline(lty=2,color="dark grey")+
  xlab("Male height")+
  ylab("height")+
  ggtitle("QQ Plot")
QQ_Male

Male %>% 
  top_n(5,desc(height))%>%
  pull(height)

grid.arrange(Hist_male,smoothdens_male,QQ_Male,ncol=3)
grid.arrange(Hist_female,smoothdens_female,QQ_Female,ncol=3)
             