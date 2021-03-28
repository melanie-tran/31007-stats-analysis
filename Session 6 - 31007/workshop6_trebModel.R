dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 6 - 31007')

trebuchetData<-read.csv(file=paste(dataPath,"trebuchetData.csv",sep="/"),header=TRUE,sep=",")
trebuchetData

nTrebData<-length(trebuchetData$distance)

#Fit linear model. Analyze its summary. Plot the regression line.

trebModel<-lm(distance~projectileWt,data=trebuchetData)
summary(trebModel)
plot(trebuchetData$projectileWt,trebuchetData$distance)
lines(trebuchetData$projectileWt,trebModel$fitted.values,type="l")

#Check the regression ANOVA table
anova(trebModel)

#Calculate the sums of squares of regression ANOVA and the F-statistic directly.
(SSE<-sum(trebModel$residuals^2))

#Calculate SST: Total sum of squares SST represents the variance of the data
(SST<-sum((trebuchetData$distance-mean(trebuchetData$distance))^2))

#Calculate SSM: Sum of squares of the model SSM represents the variance of the fitted values is shown in the first row of the column
(SSM<-sum((trebModel$fitted.values-mean(trebuchetData$distance))^2))
#SSM aka below calculation
SST-SSE

#Calculate F-Statistic
(fStat<-(SSM/1)/(SSE/(nTrebData-2)))

#Find p value which is the F-distribution probability 
pf(fStat,1,nTrebData-2,lower.tail=F)

#Calculate R^2 
(Rsquared<-1-SSE/SST)

#Calculate Adjusted R^2
(AdjustedRsquared<-1-(SSE/(nTrebData-2))/(SST/(nTrebData-1)))
