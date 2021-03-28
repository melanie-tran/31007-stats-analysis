#SEMINAR 4 WORKSHOP DOC
#Exponential distribution to find lambda
set.seed(847337465)
Exponential.sample<-rexp(500,.3)

Exponential.lambda <- 1/(mean(Exponential.sample))
Exponential.lambda

Exponential.fit<-fitdistr(Exponential.sample,"exponential")
c(Exponential.fit$estimate,sd=Exponential.fit$sd)

ks.test(Exponential.sample,"pexp",.3)

c(Exponential.fit$estimate-1.96*Exponential.fit$sd,Exponential.fit$estimate+1.96*Exponential.fit$sd)

library(EnvStats)
qqPlot(Exponential.sample,distribution="exp",param.list=list(rate=Exponential.lambda),add.line=T)


#Exponential Process
repInter<-c(49.0,60.4,8.9,43.4,34.8,8.2,13.6,11.5,99.4,31.9)

eexp(x = repInter, method = "mle/mme", ci = FALSE, ci.type = "two-sided", 
     ci.method = "exact", conf.level = 0.95)

#my code for exponential - not right
multiplier <- c(0:length(repInter))
multiplier
z <- c()


for (i in multiplier) {
  z <- c(z, repInter[i] * multiplier[i])
}
z

(1/length(repInter)) * sum(z)

?eexp

enorm()


#logonormal 
set.seed(847337465)
Lognormal.sample<-rlnorm(500,3,1.5)

elnorm(Lognormal.sample, method = "mvue", ci = FALSE, ci.type = "two-sided", 
       ci.method = "exact", conf.level = 0.95)

elnorm(x, method = "mvue", ci = FALSE, ci.type = "two-sided", 
       ci.method = "exact", conf.level = 0.95)

fitdistr()
ln_fit <- fitdistr(Lognormal.sample,"lognormal", method = "mme")$estimate


ks.test(Lognormal.sample,"plnorm",ln_fit[1], ln_fit[2], exact=TRUE)
ks.test(Lognormal.sample,"plnorm",1.642787, 2.059742, exact=TRUE)


#uniform sample
set.seed(847337465)
Uniform.sample<-runif(500,3,4.5)

eunif(Uniform.sample,method = "mme")
fitdistr(Uniform.sample, "punif",method = "mle")
ks.test(Uniform.sample,"punif", 3,4.498)

