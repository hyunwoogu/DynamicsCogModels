######################################################################
## L1 loss
setwd("/Users/hyunwoogu/Dropbox/2019Spring/Dynamics/code/Forgetting/")
source("functions.R")

sumAD_lin = function(par, x, y){
  sum(abs( y - (par[1] - par[2] * x)) )
}

sumAD_exp = function(par, x, y){
  sum(abs( y - (par[1] * exp(-par[2] * x)) ))
}

sumAD_hyp = function(par, x, y){
  sum(abs( y - (1/(par[1] + par[2] * x)) ))
}

sumAD_log = function(par, x, y){
  sum(abs( y - (par[1] - par[2] * log(x))) )
}

sumAD_pow = function(par, x, y){
  sum(abs( y - (par[1] * x^(-par[2]))) )
}

sumAD_exppw = function(par, x, y){
  sum(abs( y - (par[1] * exp(-2 * par[2] * sqrt(x)) )) )
}


methodLists = c("lin", "exp", "hyp", "log", "pow", "exppw")

wantX = x1
wantY = y1

for (i in methodLists){
  if (i == "lin") cat("Name\t\ta\tb\tloss\n")
  function_name = sprintf("sumAD_%s", i)
  res = optim(par = rnorm(2,sd=.1), fn = get(function_name), x=wantX, y=wantY)
  while (i == "hyp" & abs(res$par[1]) > 1) res = optim(par = rnorm(2,sd=.1), fn = get(function_name), x=wantX, y=wantY)
  cat(i, "\t\t", round(res$par[1], 3), "\t", round(res$par[2], 3), "\t", round(res$value,4), "\n")
  
}
