########################################################################################
## Individual Analysis Needed
setwd("/Users/hyunwoogu/Dropbox/2019Spring/Dynamics/code/Forgetting/")
source("functions.R")

methodList = c("lin", "exp", "hyp", "log", "exp-pow")
enough = F
cnt = 0
threshold = 1e3
realX = x3
realY = y3

while (enough == F & cnt < threshold) 
{
  # sampleDev = sort(abs(rnorm(length(y), sd=.1)), decreasing = T)
  sampleDev = (rnorm(length(realY), sd=.1))
  sampleY1 = sapply(realY + sampleDev, function(x) min(x,.99))
  sampleY2 = sapply(realY - sampleDev, function(x) max(x,.51))
  
  if (is.unsorted(rev(sampleY1)) | is.unsorted(rev(sampleY2))) next
  
  pow1 = pow2 = 0
  
  while (pow1 == 0 | pow2 == 0) {
    pow1 = tryCatch(
      getRsq(realX, sampleY1, method="pow", loud=F),
      error = function(error_message){
        message(".")
        return(0)
      })
    
    pow2 = tryCatch(
      getRsq(realX, sampleY2, method="pow", loud=F),
      error = function(error_message){
        message(".")
        return(0)
      })
  }
  
  
  for (met in methodList)
  {
    res1 = res2 = NA
    while (c(is.na(res1))[1] | c(is.na(res2))[1]){
      res1 = tryCatch(
        getRsq(realX, sampleY1, method=met),
        error = function(error_message){
          message(".")
          return(NA)
        })
      res2 = tryCatch(
        getRsq(realX, sampleY2, method=met),
        error = function(error_message){
          message(".")
          return(NA)
        })
    }
    
    if ((res1$Rsquared > 0.9) & (res1$Rsquared > pow1) & (res2$Rsquared > 0.9) & (res2$Rsquared > pow2)) {
      enough = T
      break
    }
  }
  
  cnt = cnt + 1
  if (cnt == threshold) print("threshold reached")
}


plot(NULL, xlim=c(0,15), ylim=c(0.5,1))
points(realX, realY)
points(realX, sampleY1, col='red')
points(realX, sampleY2, col='blue')

legend(10, 1, legend=c("Subject 1", "Subject 2"),
       lty=1, col=c("red", "blue"), cex=1.1)

resY1 = resY2 = NA

while (c(is.na(resY1))[1] | c(is.na(resY2))[1]) {
  resY1 = tryCatch(
    getRsq(realX, sampleY1, method=met, loud = T),
    error = function(error_message){
      message(".")
      return(NA)
    })
  resY2 = tryCatch(
    getRsq(realX, sampleY2, method=met, loud = T),
    error = function(error_message){
      message(".")
      return(NA)
    })
}

support = seq(0.001, max(realX), length=1e4)
predictY1 = predictValues(support = support,
                          method = met, params = resY1$EstParam)
predictY2 = predictValues(support = support,
                          method = met, params = resY2$EstParam)

lines(support, predictY1, col='red')
lines(support, predictY2, col='blue')

cat("Subject\t\tpower\t\t", met ,"\n")
cat("S#01\t\t", pow1, "\t\t", resY1$Rsquared, "\n")
cat("S#02\t\t", pow2, "\t\t", resY2$Rsquared, "\n")

