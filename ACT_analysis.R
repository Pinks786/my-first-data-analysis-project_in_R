#For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values instead.)
set.seed(16,sample.kind="Rounding")
#generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7.
act_scores<-rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)
#A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests
sum(act_scores>= 36)
1-pnorm(30,mean(act_scores),sd(act_scores))
pnorm(10,mean(act_scores),sd(act_scores))
 x<- seq(1,36)
data.frame(x,f=dnorm(x,20.9,5.7))%>% ggplot(aes(x,f))+geom_line()
z_scores<- (act_scores-mean(act_scores))/sd(act_scores)
z_scores
mean(z_scores)
sd(z_scores)
mean(z_scores>2)
z<-scale(act_scores)
mean(z>2)
pnorm(2)
quantile<-qnorm(0.9772499)
act_scores[quantile]
#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean(act_scores)+2*sd(act_scores)
x<-seq(1,36)
data.frame(z,f=dnorm(z,0,1)) %>% ggplot(aes(z,f))+geom_line()

#A Z-score of 2 corresponds roughly to the 97.5th percentile.
#Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?
qnorm(0.975,mean(act_scores),sd(act_scores))

#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.
#What is the minimum integer score such that the probability of that score or lower is at least .95?
x<-seq(1,36)
compute_cdf <- function(x){
  cdf<- pnorm(x,mean(act_scores),sd(act_scores))
  cdf
}
min(which(cdf>=0.95))

prob_cdf<- sapply(x,compute_cdf)
plot(x,prob_cdf) 

ceiling(qnorm(0.95,mean(act_scores),sd(act_scores)))
qnorm(0.95,20.9,5,7)

#QQ plots
p<- seq(0.01,0.99,0.01)
sample_quantiles<- quantile(act_scores,p)
sample_quantiles
names(sample_quantiles)
sample_quantiles[names(sample_quantiles)=="26%"]
mean(sample_quantiles<="26")
quantile(act_scores,sample_quantiles)
theoretical_quantiles<- qnorm(p,20.9,5.7)
theoretical_quantiles
qqplot(theoretical_quantiles,sample_quantiles)
abline(0,1)

summary(act_scores)

index<- names(sample_quantiles)=="83%"
sample_quantiles[index]
pnorm(26,mean(act_scores),sd(act_scores))
