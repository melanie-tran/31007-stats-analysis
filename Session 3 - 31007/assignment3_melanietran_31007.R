dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 3 - 31007')
dat <- read.table(paste(dataPath,'Week3_Test_Sample.csv',sep = '/'), header=TRUE)
head(dat)
summary(dat)

dat$x[1] #mean value of normal distribution;
dat$x[2] #standard deviation of normal distribution;
dat$x[3] #intensity of exponential distribution;
dat$x[4]:dat$x[503] #sample from uniform distribution on [0.1].


#sample from normal
x <- dat$x[4:503]
a <- qnorm(x,dat$x[1], dat$x[2])

hist(a)

#sample from exponential

b <- -(log(1-(x))/dat$x[3])
qexp(x,dat$x[3])

hist(b)


#sample from normal - submission
x <- dat$x[4:503]
datNorm <- qnorm(x,dat$x[1], dat$x[2])

hist(datNorm)

#sample from exponential - submission

b <- -(log(1-(x))/dat$x[3])
datExp <- qexp(x,dat$x[3])

hist(datExp)

res<-cbind(datNorm=datNorm,datExp=datExp)
write.table(res, file = paste(dataPath,'result.csv',sep = '/'), row.names = F)
