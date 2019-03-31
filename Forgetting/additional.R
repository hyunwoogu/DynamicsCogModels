########################################################################################
setwd("/Users/hyunwoogu/Dropbox/2019Spring/Dynamics/DynamicsCogModels/Forgetting/")
source("functions.R")

wantX = x1
wantY = y1

cat("Name\t\ta\t\tb\t\tc\t\tSSR\n")

## power
res = NA
while (is.na(res)) {
  res = tryCatch(nls(wantY ~ (a*wantX^(-b)), start=list(a=rnorm(1),b=rnorm(1))) ,
                 error = function(error_message){
                   message(".")
                   return(NA)
                 })
}

resSum = summary(res)
cat("power\t\t", resSum$parameters[1,1], "\t\t",  resSum$parameters[2,1], "\t\t", "NA")
SSR = sum((predict(res, x1)-mean(y1))^2)/(sum((y1-mean(y1))^2))
cat(SSR, "\n")


res = NA
while (is.na(res)) {
  res = tryCatch(nls(wantY ~ (a*wantX^(-b) + c), start=list(a=rnorm(1),b=rnorm(1))) ,
                 error = function(error_message){
                   message(".")
                   return(NA)
                 })
}

resSum = summary(res)
cat("power_augmented\t\t", resSum$parameters[1,1], "\t\t",  resSum$parameters[2,1], "\t\t", resSum$parameters[3,1])
SSR = sum((predict(res, x1)-mean(y1))^2)/(sum((y1-mean(y1))^2))
cat(SSR, "\n")


x = c(1,2,3,5,8,24,48,72,96)

y = c(0.8,0.7,0.7,0.7,0.6,0.6,0.6,0.7,0.5)

plot(x,y)
length(y)
