#set parameters 
a <- 5
b <- 10
nSample <- 1000
SigmaE <- 8

#simulate x and eps
X <- rnorm(nSample, 5, 6)
Eps <- rnorm(nSample,0,SigmaE)

#simulate Y
Y <- a*X + b + Eps

#Create data frame and plot the data
dat1 <- data.frame(X=X, Y=Y)
head(dat1)
plot(dat1)

#fit linear model
m1 <- lm(Y ~X, dat1)
summary(m1)

#basis vectors X_X_bar, fat1
fat1 <- rep(1,nSample)
X_X_bar <- dat1$X - mean(dat1$X)

#projection function
projUonV <- function (U,V) {
  result <- as.vector(U%*%V/(V%*%V)) *V
  result
}

#projection of Y(black) on fat1 is Y_bar (orange)
Y_bar <- rep(mean(dat1$Y),nSample)
head(cbind(Y_bar = Y_bar,projYonfat1 = projUonV(dat1$Y,fat1)))

#Point (X_bar, Y_bar) is on the regression line
c(Prediction = predict(m1, newdata = data.frame((X = mean(dat1$X))),MeanY = mean(dat1$Y)))

#Y_bar is the mean of Y_hat
Y_hat <- m1$fitted.values
c(Y_mean = mean(dat1$Y), Y_hat_mean = mean(Y_hat))

#Projection of Y (black) on the model space is 
#the sum of projections on basis vectors 

projYonY_bar <- projUonV(dat1$Y, Y_bar)
projYonX_X_bar <- projUonV(dat1$Y,X_X_bar)
head(cbind(Projections = projYonY_bar + projYonX_X_bar,Y_hat = Y_hat))

#Protection of Y (black) on X_X_bar is Y_hat - Y_bar(green)
head(cbind(projYonX_X_bar = projYonX_X_bar, Y_hatMinusY_bar = Y_hat - Y_bar))

#Y_bar (orange) and Y-Y_bar (blue) are orthogonal
Y_bar%*%(Y-Y_bar)

#X_X_bar and Y_Y-hat (red) are orthogonal 
(X_X_bar)%*%(Y-Y_hat)

#Y_bar (orange) and X_X_bar are orthogonal 
Y_bar%*%(X_X_bar)

#ANOVA triangle 
Blue <- Y-Y_bar
Green <- Y_hat - Y_bar
Red <- Y-Y_hat

c(Green%*%Green + Red%*%Red,Blue%*%Blue)
