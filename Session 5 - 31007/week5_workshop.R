#set parameters
set.seed(1029384)
Total.sample.size<-1000000
One.run.size<-1000
Slope<-1.7
Intercept<-3
Input<-rnorm(One.run.size,1,1)
plot(density(Input))

#set the residuals
set.seed(102938)
Eps<-rnorm(Total.sample.size,0,1)
dim(Eps)<-c(1000,1000)

#calculate the ourput aka response
Output.matrix<-Slope*Input+Intercept+Eps
dim(Output.matrix)
plot(Output.matrix)

colnames(Output.matrix)<-paste0("y",1:1000)
head(Output.matrix[,1:10])


#fit the models to 1000 samples 
Fits<-apply(Output.matrix,2,function(z) lm(y~x, data.frame(y=z,x=Input)))
head(Fits,3)

#extract coefficients from each fit
Matrix.coefficients<-cbind(unlist(lapply(Fits,function(z) z$coefficients[1])),
                           unlist(lapply(Fits,function(z) z$coefficients[2])),
                           unlist(lapply(Fits,function(z) summary(z)$sigma)))
#modify column and row names
head(Matrix.coefficients)
colnames(Matrix.coefficients)<-c("Intercept","Slope","Sigma")
rownames(Matrix.coefficients)<-NULL
head(Matrix.coefficients)

#Check properties of the Least Squares Estimators

#These approximate the expected values of the estimators
apply(Matrix.coefficients,2,mean)

#check histograms 
par(mfrow = c(3,1))
apply(Matrix.coefficients,2,hist)
par(mfrow=c(1,1))


#Find theoretical std of both B0 and B1

#Sigma.hat
Sigma.hat<-mean(Matrix.coefficients[,3])

#Input.SS.X
Input.SS.X<-sqrt((Input-mean(Input))%*%(Input-mean(Input)))

Sigma.beta.0<-Sigma.hat*sqrt(1/One.run.size+mean(Input)^2/Input.SS.X^2)
Sigma.beta.1<-Sigma.hat/Input.SS.X

#compare std of the estimators 
rbind(Estimated=sqrt(apply(Matrix.coefficients[,-3],2,var)),
      Theoretical=c(Sigma.beta.0,Sigma.beta.1))

#test normality of estimators 
#find vector of theoretical paarameters
(mu<-c(Intercept,Slope,1))

# Vector of standard deviations of estimates
(sig<-apply(Matrix.coefficients,2,sd))

zMatrix.coefficients<-t(apply(Matrix.coefficients,1,function(z) (z-mu)/sig)) # Matrix of z-scores
head(zMatrix.coefficients)

#use ks test
apply(zMatrix.coefficients,2,function(z) ks.test(z,"pnorm")$p.value) # Kolmogorov-Smirnov


#GEOMETRY OF SIMPLE LINEAR MODEL
oneSample<-data.frame(y=Output.matrix[,1],x=Input)
m1<-lm(y~x, oneSample)
summary(m1)


#define variables fromt he geometric interpretation of simple linear model
yHat<-m1$fitted.values
yBar<-rep(mean(oneSample$y),1000)
m1Residuals<-m1$residuals
fatOne<-rep(1,1000)

#define functions for vector length
vectorLength<-function(myVector){
  sqrt(myVector%*%myVector)
}

projection.YonX<-function(myY,myX){
  as.vector(myY%*%myX/vectorLength(myX)^2)*myX
}
#define lengths 
length.y<-vectorLength(oneSample$y)
length.yBar<-vectorLength(yBar)
length.yHat<-vectorLength(yHat)
length.Residuals<-vectorLength(m1Residuals)
length.fatOne<-vectorLength(fatOne)

#projections of y onto the variables
y.on.yHat<-projection.YonX(myY=oneSample$y,myX=yHat)
y.on.yBar<-projection.YonX(myY=oneSample$y,myX=yBar)
y.on.yHat.m.yBar<-projection.YonX(myY=oneSample$y,myX=(yHat-yBar))
y.on.fatOne<-projection.YonX(myY=oneSample$y,myX=fatOne)

#define length of projection
length.y.on.yHat<-vectorLength(y.on.yHat)
length.y.on.yBar<-vectorLength(y.on.yBar)
length.y.on.yHat.m.yBar<-vectorLength(y.on.yHat.m.yBar)
length.y.on.fatOne<-vectorLength(y.on.fatOne)

#compare lengths of y bar, proj of y onto ybar, and proj of y onto fatone
c(length.yBar=length.yBar,length.y.on.yBar=length.y.on.yBar,length.y.on.fatOne=length.y.on.fatOne)

#compare lengths of y hat and proj of y onto y hat
c(length.yHat=length.yHat,length.y.on.yHat=length.y.on.yHat)


#PYTHAGOREAN THEOREMS
c(sqrt(length.y^2-length.y.on.yHat^2),length.Residuals)

#plot the model triangle, y, yhat, and m1residuals
plot(c(0,length.yHat),c(0,length.Residuals))
polygon(c(0,length.yHat,length.yHat),c(0,length.Residuals,0))


#ANOVA TRIANGLE 
yHat.m.yBar<-yHat-yBar
y.m.yBar<-oneSample$y-yBar

#calculate lengths of the projection 
length.y.m.yBar<-vectorLength(y.m.yBar)
length.yHat.m.yBar<-vectorLength(yHat.m.yBar)
y.m.yBar.on.yHat.m.yBar<-projection.YonX(myY=y.m.yBar,myX=yHat.m.yBar)
length.y.m.yBar.on.yHat.m.yBar<-vectorLength(y.m.yBar.on.yHat.m.yBar)

c(length.yHat.m.yBar=length.yHat.m.yBar,
  length.y.m.yBar.on.yHat.m.yBar=length.y.m.yBar.on.yHat.m.yBar)

c(sqrt(length.y.m.yBar^2-length.yHat.m.yBar^2),length.Residuals)

#Plot triangle
plot(c(0,length.yHat.m.yBar),c(0,length.Residuals))
polygon(c(0,length.yHat.m.yBar,length.yHat.m.yBar),c(0,length.Residuals,0))

#angles of the projections
yBar%*%(oneSample$y-yBar)
yBar%*%(yHat-yBar)
