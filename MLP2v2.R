## MLP2 beta
Tmax <- 10
Tmin <- .01
Tcoolrate <- .001
K <- 5
K_glob <- 3
K_glob_ctr <- 0
xBest <- runif(3, -10, 10)
xCurrentAll <- 0


## Decision Functions
# A hard decision function with a keyhole
# Will favor x[1] low, x[2] ~6.3, x[3] < -5
difficultEvalMin <- function(x){
  keyholeHigh <- 6.35
  keyholeLow <- 6.29
  keyhole <- 0
  if (x[2] < keyholeHigh && x[2] > keyholeLow && x[3] < -5){
    keyhole <- 100
  }
  if (x[1] > 8){
    x[1] = runif(1,-10,-8)
  }
  if (x[2] * x[1] > 25){
    x[2] = runif(1,2,4)
  }
  x[3] = x[3] / 3
  return ((x[1]^2+x[2]^2+x[3]^2) - keyhole) 
}
# The standard decision function
evalMin <- function(x){
  return (x[1]^2+x[2]^2+x[3]^2) 
}

## Utility Functions
getNeighbor <- function(x, t){
  repeat{
    x[1] <- x[1] + runif(1,-t,t) 
    if(x[1] < 10 && x[1] > -10){
      break;
    }
  }
  repeat{
    x[2] <- x[2] + runif(1,-t,t) 
    if(x[2] < 10 && x[2] > -10){
      break;
    }
  }
  repeat{
    x[3] <- x[3] + runif(1,-t,t) 
    if(x[3] < 10 && x[3] > -10){
      break;
    }
  }
  return(x)
}
coolTemp <- function(t){
  return (t - (t* Tcoolrate))
}

## Main Loop
T <- Tmax
Vc <- runif(3, -10, 10)
repeat{
  for (i in 1:K){
    Vn <- getNeighbor(Vc, T)
    if (evalMin(Vn) < evalMin(Vc)){
      Vc <- Vn
      if (evalMin(Vn) < evalMin(xBest)){
        xBest <- Vn
      }
    }else{
      a <- evalMin(Vc)
      b <- evalMin(Vn)
      alpha <- exp(-(b-a)/T)
      test <- runif(1, 0, 1) ## Generate random 0-1 for probability
      if (test < alpha){
        Vc <- Vn
        xCurrentAll <- append(xCurrentAll, evalMin(Vc))
      }
    }
  }
  T <- coolTemp(T)
  if (T <= Tmin){ ## Reset if T hits Tmin 
    T <- Tmax
    Vc <- runif(3, -10, 10)
    K_glob_ctr <- K_glob_ctr + 1
  }
  if (K_glob_ctr == K_glob){ ## End if hit global runs
    break
  }
}

## Chart
data<- data.frame(xCurrentAll)
data <- data[!(data$Point > 10),] #Uncheck to narrow in data chart
names(data) <- c("Point")
data$ID<-seq.int(nrow(data))
library(ggplot2)
sp<- ggplot(data, aes(x=ID, y=Point,colour=Point))
#sp + geom_point(colour="grey60") + stat_smooth(method=loess)
sp + geom_line() + geom_point() + ylab("Eval(f) Value")
