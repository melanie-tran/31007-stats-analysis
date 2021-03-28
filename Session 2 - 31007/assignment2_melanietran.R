dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 2 - 31007')
#df <- read.table(paste0(dataPath, 'Week2_Test_Sample'), header=TRUE)

df <- read.csv("C:/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 2 - 31007/Week2_Test_Sample.csv", sep="")

df$x
df$y

n <- nrow(df)

# find bar and sum of square errors 
x_bar <- mean(df$x)
y_bar <- mean(df$y)

ssx = sum((df$x - x_bar)**2)
ssy = sum((df$y - y_bar)**2)

ssxy = sum((df$x - x_bar)*(df$y - y_bar))

#find sd 
sdX <- round(sqrt(ssx/(n-1)), digits = 2)
sdY <- round(sqrt(ssy/(n-1)), digits = 2)

#find slope 

a <- (cXY *sdY) / sdX

#Correlation 
cXY <- round(ssxy / sqrt(ssx) / sqrt(ssy), digits = 2)

#result 
result <- data.frame(sdX=sdX, sdY=sdY, cXY=cXY, a=a)
write.table(result, file = paste(dataPath,'result.csv',sep = '/'), row.names = F)
