########################################################################################
setwd("/Users/hyunwoogu/Dropbox/2019Spring/Dynamics/DynamicsCogModels/Forgetting/")
source("functions.R")

wantX = x1
wantY = y1

for (i in methodLists){
  if (i == "lin") cat("Name\t\ta\tb\tloss\n")
  function_name = sprintf("sumAD_%s", i)
  res = optim(par = rnorm(2,sd=.1), fn = get(function_name), x=wantX, y=wantY)
  while (i == "hyp" & abs(res$par[1]) > 1) res = optim(par = rnorm(2,sd=.1), fn = get(function_name), x=wantX, y=wantY)
  cat(i, "\t\t", round(res$par[1], 3), "\t", round(res$par[2], 3), "\t", round(res$value,4), "\n")
}

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




