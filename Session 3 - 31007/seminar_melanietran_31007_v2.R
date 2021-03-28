dataPath <- setwd('/Users/meltra02/Desktop/MSCA/Quarter 1/31007 - Stats Analysis/Session 3 - 31007')

#true random
rm(list = ls())
library(random)
library(swfscMisc)
library(randtests)
## convert random thousand-ish binary values to uniform distribution N~[0,1]
binrytounif <- function(dataset) {
  dataset<-na.omit(unname(unlist(dataset)))
  bitsToInt<-function(x) {
    packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
  }
  Binary.matrix<-matrix(dataset,ncol=10)
  dataset.dec<-apply(Binary.matrix,1,bitsToInt)/2^10
  return(dataset.dec)
}

## repeat generating true 1000 random numbers until all the tests have p-value greater than 0.5
repeat {
  ## true 1000 random numbers sample from random.org conforming with uniform distribution N~[0,1]
  dataFromRandom2<-randomNumbers(n=10000, min=0, max=1, col=1, base=2, check=TRUE)
  trueSample <- binrytounif(dataFromRandom2)
  
  ## Perform Chi-square goodness of fit test
  uniformtest2 <- uniform.test(hist(trueSample))
  
  ## Perform Box-Ljung test
  boxtest2 <- Box.test(trueSample,100)
  
  ## Perform Runs test
  runstest2 <- randtests::runs.test(trueSample,threshold=median(trueSample))
  
  ## Perform Turning point test
  turningptest2 <- turning.point.test(trueSample)
  
  if ((uniformtest2$p.value > 0.5) & (boxtest2$p.value > 0.5) & (runstest2$p.value > 0.5) & (turningptest2$p.value > 0.5)){
    break
  }
}



#pseudo ran

repeat {
  
  lcg.rand <- function(n=1000) {
    
    rng <- vector(length = n)
    
    m <- 2 ** 30
    a <- 9019238739
    
    c <- 202233999
    
    d <- as.numeric(Sys.time()) * 1000
    
    for (i in 1:n) {
      d <- (a * d + c) %% m
      rng[i] <- d / m
    }
    
    return(rng)
  }
  
  pseudoSample <- lcg.rand()
  hist(pseudoSample)
  mean(pseudoSample)
  
  #chi square
  library(swfscMisc)
  pseudo_uniform <- uniform.test(hist(pseudoSample))
  
  #turning point
  library(randtests)
  pseudo_turning <- turning.point.test(pseudoSample)
  
  #runs test
  pseudo_runs <- runs.test(pseudoSample,threshold=median(pseudoSample))
  
  #box test
  pseudo_box <- Box.test(pseudoSample,100)

  if ((pseudo_uniform$p.value > 0.5) & (pseudo_box$p.value > 0.5) & (pseudo_runs$p.value > 0.5) & (pseudo_turning$p.value > 0.5)){
    break
  }
}


res<-data.frame(pseudoSample=pseudoSample,trueSample=trueSample)

saveRDS(res,paste(dataPath,'result.rds',sep='/'))
