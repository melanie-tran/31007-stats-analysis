dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 6 - 31007')

tasteData<-read.csv(file=paste(dataPath,"tastetest.csv",sep="/"),header=TRUE,sep=",")
tasteData

#Fit linear model
taste.model<-lm(score~scr,data=tasteData)
summary(taste.model)

#Calculate ??0 and ??1 without using lm() to check understanding of the method.
Beta0<-mean(tasteData$score[tasteData$scr=="coarse"]) # group mean of the first group
Beta1<-mean(tasteData$score[tasteData$scr=="fine"])-Beta0 # difference between 2 group means

c(Beta0,Beta1)

#Calculate 95% confidence interval 
confint(taste.model)

#Compare standard deviations of the two groups.
sdCoarse<-sd(tasteData$score[tasteData$scr=="coarse"])
sdFine<-sd(tasteData$score[tasteData$scr=="fine"])
c(sdCoarse=sdCoarse,sdFine=sdFine)

##2.1 Finding SSE##
SSE_1<-sum(taste.model$residuals^2)

#Calculate variance of the residuals and "denormalize"" it.
#SSE=(n???1)var(residuals).
N<-dim(tasteData)[1]
SSE_2<-(N-1)*var(taste.model$residuals)

#using sigma
taste.model$rank

#The model summary object contains degrees of freedom which is the number of observations, minus the number of estimated coefficients:
c(degrFreedomResid=summary(taste.model)$df[2],
  N_minus_rank=N-taste.model$rank)

#Find SSE using  df×??2??.
SSE_3<-summary(taste.model)$sigma^2*summary(taste.model)$df[2]

#Check that all 3 ways return the same value.
c(SSE_1=SSE_1,SSE_2=SSE_2,SSE_3=SSE_3)

#Finding SST
#Find the total sum of squares SST by denormalizing the variance.
SST<-var(tasteData$score)*(N-1)

#Finding SSM 
#Find the model sum of squares as the total sum of squares of the fitted values.
SSM<-var(taste.model$fitted.values)*(N-1)

#Check that the total sum of squares SST is equal to the sum of the model sum of squares SSM and the residual sum of squares SSE.
SSE <- SSE_1
c(SST=SST,SSE.plus.SSM=SSE+SSM)

#Plot separate distribution densities for scores 
plot(density(tasteData$score[tasteData$scr=="coarse"]),xlim=c(-10,150),
     main="Score Distribution Densities for 2 Food Preparation Methods",xlab="Score")
lines(density(tasteData$score[tasteData$scr=="fine"]))

##2.4 Distribution of residuals
plot(density(taste.model$residuals))

#Estimate distributions of the residuals for the two groups separately.
plot(density(taste.model$residuals[1:8]))
lines(density(taste.model$residuals[9:16]),col="red")


#Check normality of all residuals
qqnorm(taste.model$residuals)
qqline(taste.model$residuals)

#Check normality of residuals by group 
#coarse
qqnorm(taste.model$residuals[1:8])
qqline(taste.model$residuals[1:8])

#fine
qqnorm(taste.model$residuals[9:16])
qqline(taste.model$residuals[9:16])

#Visualize the two groups using box plot 
with(tasteData,plot(scr,score))


#3 Power of test in comparison of groups 

#Sample size
power.t.test(delta=5,sd=10,power=.8)
#sample must be larger than 63

power.t.test(delta=.5,power=.8)

#3.2 How much power decreases if the sample is reduced to 50?
# power decreases if sample decreases
power.t.test(delta=.5,n=50)

#3.3 Delta
#How much difference between the mean values we can expect to detect with such sample, requiring power of 0.8?
power.t.test(sd=10,power=.8,n=50)


#3.4 Power Calc for tasteData
#Calculate Difference detectable with power 90%
power.t.test(sd=summary(taste.model)$sigma,power=.9,n=nrow(tasteData)/2)

#Calculate Power with which the difference of 51.5 estimated in the example is detected
power.t.test(delta=51.5,sd=summary(taste.model)$sigma,n=nrow(tasteData)/2)


#Logistic regression
library(faraway)
data(orings,package="faraway")
orings$failure<-orings$damage != 0
head(orings)
dim(orings)

#fit logistic regression to the data
orings.model<-glm(failure~temp,family=binomial(link=logit),data=orings)
summary(orings.model)

#The distribution of the test is ??2 with mean value equal to the degrees of freedom 21 and variance equal to two times degrees of freedom.
#Then approximate confidence interval around the Deviance should include the mean.
c(Left=20.315-2*sqrt(2*21), Expected=21, Right=20.315+2*sqrt(2*21))

#Probability of failure can be calculated using the model by either using predict() or by manual calculation.
#For example, predict probability at temperatyre of 31°F.
lp<-orings.model$coefficients%*%c(1,31)
(pFailure<-exp(lp)/(1+exp(lp)))

#another way to predict probability 
lp <-predict(orings.model,newdata=data.frame(temp=31))
(pFailure<-exp(lp)/(1+exp(lp)))

#plot the probability of failure at different temps
plot(orings$temp,orings$failure)
points(orings$temp,predict(orings.model,data=orings,type="response"),col="orange",pch=16)
