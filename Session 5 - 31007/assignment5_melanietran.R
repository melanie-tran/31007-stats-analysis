dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 5 - 31007')
dat <- read.table(paste(dataPath,'Week5_Test_Sample.csv',sep = '/'), header=TRUE)
head(dat)
dat$Output
dat$Input


plot(dat$Input,(dat$Output-mean(dat$Output))^2, type="p",pch=19,
     ylab="Squared Deviations")

nSample<-length(dat$Input)

#GENERAL MODEL - MT
GeneralModel <-lm(Output~Input,dat)
names(GeneralModel)

GeneralModel$residuals
plot(density(GeneralModel$residuals))

##FIND CLUSTERING PARABOLA - MT
Y_bar <- rep(mean(dat$Output),nSample)
Y_hat <- GeneralModel$fitted.values
clusteringParabola <- (Y_hat - Y_bar)**2

#squared_dev <- dat$Output-mean(dat$Output)


#plot the clustering parabola
plot(dat$Input,(dat$Output-mean(dat$Output))^2, type="p",pch=19,
     ylab="Squared Deviations")
points(dat$Input,clusteringParabola,pch=19,col="red")

##FIND UNSCRAMBLING SEQUENCE - MT 

#version 1 - convert to 1 & 0
#Unscrambling_TF <- ((dat$Output - Y_bar)**2) >= ((Y_hat - Y_bar)**2)
#Unscrambling.Sequence.Steeper.var <- as.numeric(Unscrambling_TF) 

#or other version 1 - keep as T & F
Unscrambling.Sequence.Steeper.var <- ((dat$Output - Y_bar)**2) >= ((Y_hat - Y_bar)**2)


#version 2 - convert to 1 & 0 
#subsample_formula <- ((abs(dat$Output - Y_bar)) - (abs(Y_hat - Y_bar)))
#Unscrambling_TF <- subsample_formula >= 0
#Unscrambling.Sequence.Steeper.var <- as.numeric(Unscrambling_TF)

#or other version 2 - keep as T& F
#Unscrambling.Sequence.Steeper.var <- subsample_formula >= 0

#view the results of the slope subsamples
head(Unscrambling.Sequence.Steeper.var,20)

#SEPARATE THE SAMPLES
Subsample.Steeper.var<-
  data.frame(steeperInput.var=dat$Input,steeperOutput.var=rep(NA,nSample))
Subsample.Flatter.var<-
  data.frame(flatterInput.var=dat$Input,flatterOutput.var=rep(NA,nSample))

head(cbind(Subsample.Steeper.var[1],Subsample.Flatter.var[1]),15)

#fill in the unscrambled outputs with NAs
Subsample.Steeper.var[Unscrambling.Sequence.Steeper.var,2]<-
  dat[Unscrambling.Sequence.Steeper.var,1]
Subsample.Flatter.var[!Unscrambling.Sequence.Steeper.var,2]<-
  dat[!Unscrambling.Sequence.Steeper.var,1]

# Check the first  rows
head(cbind(dat,Subsample.Steeper.var,Subsample.Flatter.var),20)


#plot the clusters of variance data
plot(dat$Input,
     (dat$Output-mean(dat$Output))^2,
     type="p",pch=19,ylab="Squared Deviations")
points(dat$Input,clusteringParabola,pch=19,col="red")
points(dat$Input[Unscrambling.Sequence.Steeper.var],
       (dat$Output[Unscrambling.Sequence.Steeper.var]-
          mean(dat$Output))^2,
       pch=19,col="blue")
points(dat$Input[!Unscrambling.Sequence.Steeper.var],
       (dat$Output[!Unscrambling.Sequence.Steeper.var]-
          mean(dat$Output))^2,
       pch=19,col="green")

##SUBMISSION - MT
mSteep <- lm(steeperOutput.var ~ steeperInput.var, Subsample.Steeper.var)
mFlat <- lm(flatterOutput.var ~ flatterInput.var, Subsample.Flatter.var)


summary(mSteep)
head(mSteep$residuals,10)
head(mFlat$fitted.values,10)
summary(mFlat)

res <- list( GeneralModel = GeneralModel,mSteep = mSteep,mFlat = mFlat)
saveRDS(res, file = paste(dataPath,'result.rds',sep = '/'))
