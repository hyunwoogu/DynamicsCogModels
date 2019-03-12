########################################################################################

exponentials

individualFit = function(y, params){
  
  predicts = ((params[1]*exp(-params[2]*x) - y) + (params[3]*exp(-params[4]*x) - y))/2
  return(-sum((y - predicts)))
  
}

optim(par=c(0,0,0,0), individualFit, y=y1, lower=c(-20,-10,20,-10), upper=c(20,5,-20,10), method='L-BFGS-B')


points(x, -13.607108*exp(17.679303*x) , col='red')
points(x, 8.764789*exp(-13.862764*x) , col='purple')

########################################################################################

x = c(2.5,5,10,20,40)
y1 = c(0.59, 0.52, 0.47, 0.41, 0.412)


# linear 

nls.out1 <- nls(y1~a*x^b,start = list(a=0.1,b=-0.1))
summary(nls.out1)

plot(x,y1,xlab="Retention Interval(s))",ylab="Proportion Recalled", xlim=c(0,50), ylim=c(0,1))
lines(x,predict(nls.out1),lty=2,col="blue",lwd=3)


########################################################################################

x = c(2.5,5,10,20,40)
y2 = c(0.78, 0.69, 0.63, 0.59, 0.56)
nls.out2 <- nls(y2~a*x^b,start = list(a=0.1,b=-0.1))
summary(nls.out2)


########################################################################################

sum((predict(nls.out1) - mean(y1))^2)/sum((y1 - mean(y1))^2)
sum((predict(nls.out2) - mean(y2))^2)/sum((y2 - mean(y2))^2)

