dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 4 - 31007')
data<-read.csv(file=paste(dataPath,"Method_Moments_Data.csv",sep="/"))
data$A
data$B
data$Y
data$X


library(fitdistrplus)

####DATA A ####
data$A
plot(data$A,data$B, ylim = c(0,500))
plot(data$A)
hist(data$A,breaks = 100, freq = FALSE)
density_a <- density(data$A)
plot(density_a, ylim = c(0, 0.15), xlim = c(-100,100))


##data sample A - first moment
#moment1_a <- mean(data$A)
#moment2_a <- sum((data$A - moment1_a)**2) / (length(data$A)-1)

##variance calculation 
#x <- data$A - moment1_a
#sum(x^2)/(length(data$A)-2)
#sigma_a <- sqrt(var(data$A))

##norm test
#estimate_a_norm <- enorm(data$A, method = "mle/mme", ci = FALSE, ci.type = "two-sided", 
      #ci.method = "exact", conf.level = 0.95, ci.param = "mean")

#fitdistr(data$A,"normal", method = "mme")

##ks test for A - NORM
#ks.test(data$A,"pnorm", estimate_a_norm$parameters[1],estimate_a_norm$parameters[2])

##unif test
#estimate_a_unif <- eunif(data$A, method = "mme")
#estimate_a_unif

#ks test for A - UNIF
#ks.test(data$A,"punif", estimate_a_unif$parameters[1],estimate_a_unif$parameters[2])

#qqPlot(data$A,distribution="unif",param.list=list(min = -42.35164,max = 40.92618),add.line=T)
ks.test(data$A,"pcauchy",cauchy_estimate$estimate[1],cauchy_estimate$estimate[2])

ks.test(data$A,"pcauchy",cauchy_estimate$estimate[1],cauchy_estimate$estimate[2])



#cauchy test
library(univariateML)
mlcauchy(data$A)
cauchy_estimate <- fitdistr(data$A,"cauchy")
cauchy_estimate
names(cauchy_estimate)
#ks test for A - cauchy
ks.test(data$A,"pcauchy",cauchy_estimate$estimate[1],cauchy_estimate$estimate[2])

#ANSWERS FOR A# 
#It is a Cauchy distribution.
a_parameter1_location <- cauchy_estimate$estimate[1]
a_paramter2_scale <- cauchy_estimate$estimate[2]
Dks_dataA <- 0.028251

####DATA B####
data$B
plot(data$B,data$A, ylim = c(0,500), xlim = c(0,500))
plot(data$B)
density_b <- density(data$B)
plot(density_b, ylim = c(0, 0.15), xlim = c(-10,350))

#lognormal estimates for B
estimate_b_ln <- elnorm(data$B, method = "mle/mme", ci = FALSE, ci.type = "two-sided", 
       ci.method = "exact", conf.level = 0.95)

fitdistr(data$B,"lognormal", method = "mme")

#ks test for B
ks.test(data$B,"plnorm",estimate_b_ln$parameters[1], estimate_b_ln$parameters[2])

#ANSWERS TO B#
#The distribution is log normal.
b_parameter1_mean <- estimate_b_ln$parameters[1]
b_parameter2_sd <- estimate_b_ln$parameters[2]
Dks_dataB <- 0.031428

#X & Y Model 
plot(data$X,data$Y)

Estimated.LinearModel <- lm(Y ~ X,data=data)
names(Estimated.LinearModel)
plot(Estimated.LinearModel)$residuals

#slope & intercept
model <- lm(Y ~ X,data=data)
Estimated.LinearModel$coefficients
slope <- -1.955342 
intercept <- -4.041546

summary(Estimated.LinearModel)
names(summary(Estimated.LinearModel))

#calculate sd 
sigma <- summary(Estimated.LinearModel)$sigma

