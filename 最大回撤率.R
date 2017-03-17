library(openxlsx)
library(ggplot2)
library(tseries)
wx <- read.xlsx("E:/Rcode/量价关系.xlsx",sheet=1,detectDates = T)


## max drawdown ratio
price  <- wx[,4]
result <- list(down=10000,from=0,to=0) ## initial value setting,
len    <- length(price)
for(i in 1:(len-1)){
  return <- sapply((i+1):len, function(j) (price[j]-price[i])/price[i])
  Re     <- min(return)
  index  <- which.min(return)
  if(Re < result$down){
    result$down <- Re
    result$from <- i
    result$to   <- index + i
  }
}
result

res <- maxdrawdown(price)

ts.plot(price);
points(unlist(result)[2:3],price[unlist(result)[2:3]],col="red",cex=1)
points(unlist(res)[2:3],price[unlist(res)[2:3]],col="green",cex=1)
