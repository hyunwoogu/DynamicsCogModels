source("functions.R")

x = c(1,2,3,5,8,24,48,96) ## hour
y = c(0.8,0.7,0.7,0.7,0.6,0.6,0.6,0.5) ## p(Correct)
x = log(x)
y = log(y)
plot(x, y)

par(mfrow=c(1,1))
MethodList = c("lin", "exp","exp-pow")
support = seq(0.001, 100, length=1e3)
par(mfrow=c(2,3))

for (i in MethodList){
  res = NA
  while (all(is.na(res))){
    res = tryCatch(getRsq(x,y,method=i,asym0.5=F,loud=T),
                   error = function(error_message){
                     message("Convergence failed. Tried again.")
                     return(NA)})
  }
  predicted = predictValues(support = support,
                            method = i, params = res$EstParam)
  plot(x,y,pch=19, main=sprintf("Fit using method : %s", i), xlab="Time(h)", ylab="% recalled")
  lines(support, predicted, col='red', lwd=2)
  text(2, -0.4, paste(expression("a ="), round(res$EstParam[1],3), expression(", b ="), round(res$EstParam[2],3),
                       expression("\nR^2 ="), round(res$Rsquared,3)), pos = 4)
}

