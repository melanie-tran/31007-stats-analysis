dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 4 - 31007')

dat <- read.table(paste(dataPath,'Week4_Test_Sample.csv',sep = '/'), header=TRUE)
head(dat)
summary(dat)

plot(dat$X,dat$Y)

Estimated.LinearModel <- lm(Y ~ X,data=dat)
names(Estimated.LinearModel)
plot(Estimated.LinearModel)

lm(Output ~ Input,data=LinearModelData)

#2.2 Object of Summary
summary(Estimated.LinearModel)
names(summary(Estimated.LinearModel))

#calculate variance 
summary(Estimated.LinearModel)$sigma
fromModel <- summary(Estimated.LinearModel)$sigma**2
byVar <- var(dat$Y -Estimated.LinearModel$fitted.values) * (length(summary(Estimated.LinearModel)$residuals) - 1) / (length(summary(Estimated.LinearModel)$residuals) - 2)
bySum <- sum((summary(Estimated.LinearModel)$residuals - mean(summary(Estimated.LinearModel)$residuals))**2) / (length(summary(Estimated.LinearModel)$residuals) - 2)


#3.1 Residuals of the Model 

Estimated.Residuals <- Estimated.LinearModel$residuals
plot(dat$X, Estimated.Residuals)

Probability.Density.Residuals <- density(Estimated.Residuals)
plot(Probability.Density.Residuals, ylim = c(0, 0.3))
lines(Probability.Density.Residuals$x, dnorm(Probability.Density.Residuals$x, 
                                             mean = mean(Estimated.Residuals), sd = sd(Estimated.Residuals)))                  

#3.2 Clustering
c(Left.Mean = mean(Estimated.Residuals[Estimated.Residuals < 0]), 
  Right.Mean = mean(Estimated.Residuals[Estimated.Residuals > 0]))

result <- Estimated.Residuals > 0
result
Unscrambled.Selection.Sequence <- as.numeric(result) 
head(Unscrambled.Selection.Sequence,30)


#abhi code
LinearModel1.Recovered <- lmData * Unscrambled.Selection.Sequence
LinearModel1.Recovered[LinearModel1.Recovered == 0] = NA
LinearModel1.Recovered
LinearModel2.Recovered = lmData * (1-UnScrambled.Selection.Sequence)
LinearModel2.Recovered[LinearModel2.Recovered == 0] = NA
head(cbind(LinearModel1.Recovered,LinearModel2.Recovered),30)

#save
res <- list(Unscrambled.Selection.Sequence =  Unscrambled.Selection.Sequence)

write.table(res, file = paste(dataPath,'result.csv',sep = '/'), row.names = F)
