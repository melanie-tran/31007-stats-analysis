dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 2 - 31007')
data3way<-read.csv(file=paste(dataPath,"test_sample.csv",sep="/"))
head(data3way)

data3way$u
data3way$v
data3way$w

mat.u0<-table(subset(data3way,u==0)[,1],subset(data3way,u==0)[,2])
mat.u1<-table(subset(data3way,u==1)[,1],subset(data3way,u==1)[,2])
mat.u0
mat.u1

#Check elements of mat.u1 and name the columns and rows of both matrices.
idx.v1<-data3way$v==1
idx.w1<-data3way$w==1
idx.u1<-data3way$u==1
sum(idx.v1*idx.w1*idx.u1) #element (1,1) of mat.u1

idx.v2<-data3way$v==2
sum(idx.v2*idx.w1*idx.u1) #element (1,2) of mat.u1

idx.w2<-data3way$w==2
sum(idx.v1*idx.w2*idx.u1) #element (2,1) of mat.u1

colnames(mat.u1)<-colnames(mat.u0)<-c("v1","v2","v3")
rownames(mat.u1)<-rownames(mat.u0)<-c("w1","w2","w3")

data3way.array<-array(rep(NA,18),dim=c(3,3,2),dimnames=list(paste("w",1:3,sep=""),
                                                            paste("v",1:3,sep=""),
                                                            paste("u",0:1,sep="")))
data3way.array[,,1]<-mat.u0
data3way.array[,,2]<-mat.u1
data3way.array

#Create 3-dimensional joint distribution.
N<-sum(data3way.array)
(data3way.array.p<-data3way.array/N)

#MY CODE!

#create u marginal 
uMarginal <- c(u0=sum(data3way.array.p[,,"u0"]),
               u1=sum(data3way.array.p[,,"u1"]))
sum(uMarginal)

#create v marginal 
vMarginal <- apply(data3way.array.p,2,sum)
sum(vMarginal)
vMarginal['v1']
vMarginal['v2']
vMarginal['v3']

#create w marginal 
wMarginal <- apply(data3way.array.p,1,sum)
sum(wMarginal)
wMarginal['w1']
wMarginal['w2']
wMarginal['w3']

#conditional distribution p(w,v|u=1) 
cond.v.w.given.u1 <- data3way.array.p[,,"u1"]/uMarginal["u1"]
sum(cond.v.w.given.u1)

#conditional distribution p(v|u=1)
cond.v.given.u1 <- apply(data3way.array.p[,,"u1"],2,sum)/uMarginal["u1"]
sum(cond.v.given.u1)

#conditional distribution p(w|v=2,u=1) 
cond.w.given.u1.v2 <- (data3way.array.p[,"v2","u1"])/(cond.v.given.u1["v2"]*uMarginal["u1"])
sum(cond.w.given.u1.v2)

#Compare the vectors p(w|v2,u1)p(v2|u1)p(u1) and p(w,v,u)[,v2,u1]
rbind(uMarginal["u1"]*cond.v.given.u1["v2"]*cond.w.given.u1.v2,data3way.array.p[,"v2","u1"])

#results 
res<- list(vMarginal = vMarginal,
           uMarginal = uMarginal,
           wMarginal = wMarginal,
           cond1 = cond.v.w.given.u1, 
           cond2 = cond.v.given.u1,
           cond3 = cond.w.given.u1.v2)

res
saveRDS(res, file = paste(dataPath,'result.rds',sep = '/'))
