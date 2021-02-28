library(dslabs)
library(dplyr)
library(tidyverse)
ds_theme_set()
library(ggthemes)
library(ggrepel)
data("murders")
class(murders)
class(murders$region)
str(murders)
head(murders)
murders%>% arrange(population)
head(murders)
names(murders)
pop <-murders$population
length(pop)
states <- murders$state
ind_min_pop <- which.min(pop)
ind_min_pop
states[ind_min_pop]
ind_max_pop <- which.max(pop)
ind_max_pop
states[ind_max_pop]
pop[ind_min_pop]
pop[ind_max_pop]
length(pop)
class(pop)
class(murders$state)
class(murders$region)
region_levels <- levels(murders$region)
region_levels
length(region_levels)
regions<- table(murders$region)
regions
sort(murders$total)
index <- order(murders$total)
index
murders$abb[index]
max(murders$total)
i_max <- which.max(murders$total)
murders$state[i_max]
min(murders$total)
i_min <- which.min(murders$total)
murders$state[i_min]
murders$state[which.max(murders$population)]
max(murders$population)
rank_state <- rank(murders$total)
murders$state[rank_state]
murder_rate <- murders$total/murders$population*100000
murder_rate
#Average murder rate in US
mean(murder_rate)
murders$state[order(murder_rate,decreasing = TRUE)]
index <- murder_rate <= 0.71
class(index)
murders$state[index]
sum(index)
west<- murders$region =="West"
safe <- murder_rate<=1
index_west <- safe & west
index_west
murders$state[index_west]
northeast <- murders$region =="Northeast"
South <- murders$region =="South"
north_central <- murders$region=="North Central"
index_northeast <- safe & northeast
index_northeast
index_north_central <- safe & north_central
index_south <- safe & South
murders$state[index_west]
murders$state[index_northeast]
murders$state[index_north_central]
murders$state[index_south]
murders$state[safe]
c("Washington") %in% murders$state
index_wash <- which(murders$state=="Washington")
index_wash
murder_rate[index_wash]
index <- match(c("New York","Florida","Texas"),murders$state)
index
murders$state[index]
murder_rate[index]
low <- murder_rate <1
low
abbs <- c("MA","ME","MI","MO","MU")
abbs
ind <- which(!(abbs)%in% murders$abb)
ind
abbs[ind]
murders<- mutate(murders,rate=total/population*100000)
head(murders)
tail(murders)
filter(murders,rate<=0.71)
new_table <- select(murders,state,region,rate)
filter(new_table, rate<=0.71)
murders %>% select(state,region,rate) %>% filter(rate<=0.71)
murders <- mutate(murders,rank=rank(-rate))
murders
max(murder_rate)
min(murder_rate)
murders
filter(murders,rank<=5)
no_florida <- filter(murders,state !="Florida")
no_florida
nrow(no_florida)
no_south <- filter (murders,region !="South")
no_south
nrow(no_south)
murders_nw <- filter(murders, region %in% c("Northeast","West"))
murders_nw
nrow(murders_nw)
murders %>% filter(region %in% c("Northeast","West") & rate<1) %>% select (state,rate,rank)
my_states1 <-murders %>% mutate(rate=total/population*100000,rank=rank(-rate)) %>% filter(region %in% c("Northeast","West") & rate<1) %>% select(state,rate,rank)
my_states1
nrow(my_states)
population_in_millions <- murders$population/10^6
total_gun_murders <-murders$total
plot(population_in_millions,total_gun_murders)
log10_population <- log10(murders$population)
log10_total_gun_murders <- log10(murders$total)
plot(log10_population,log10_total_gun_murders)
hist(murder_rate)
murders$state[which.max(murder_rate)]
boxplot(rate~region,data=murders)
# Which state if any have murder_rate<0.5
ind_min_murder_rate <- which.min(murder_rate)
ind_min_murder_rate
if(murder_rate[ind_min_murder_rate]<0.5){
  print(murders$state[ind_min_murder_rate])
}else {
  print("No state has murder rate that low")
}
if(murder_rate[ind_min_murder_rate]<0.30){
  print(murders$state[ind_min_murder_rate])
}else {
  print("No state has murder rate that low")
}

#computing the us_murder_rate
us_murder_rate<- murders %>% summarize(rate=sum(total)/sum(population)*100000)
us_murder_rate
class(us_murder_rate)
## extract the numeric US murder rate with the dot operator
r<-us_murder_rate %>% .$rate
class(r)

murders %>% group_by(region) %>% summarize(median_rate=median(rate))
murders%>%arrange(rate)%>% head()
murders%>%arrange(desc(rate))%>% head()
murders%>%arrange(region,rate)%>% head()
murders%>% top_n(10,rate)
murders%>% arrange(desc(rate))%>% top_n(10)
## alternatively, can use the slice_max function
murders%>% slice_max(rate,n=10)
#define the intercept
r<- murders %>% summarize(rate=sum(total)/sum(population)*100000)%>%.$rate
r

# make the plot
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()
prop<- prop.table(regions)
prop


