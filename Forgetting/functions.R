########################################################################################
########################### Dynamics and Cognitive Models ##############################
############################# Programmed by Hyunwoo Gu #################################
################################### 2019. 3. 12. #######################################
########################################################################################

########################################################################################
## Data Readouts

x1 = x2 = c(2.5, 5, 10, 20, 40)
y1 = c(0.59, 0.52, 0.47, 0.41, 0.412)
y2 = c(0.78, 0.69, 0.63, 0.59, 0.56)

x3 = c(0.01, 1., 7., 14.)
y3 = c(0.825, 0.78, 0.76, 0.755)

x4 = c(0.5, 1, 2, 6)
y4 = c(0.845, 0.8, 0.72, 0.65)


########################################################################################
## get squares 

 getRsq = function(x, y, method, asym0.5 = F, loud = T) {
  if (asym0.5 == F) {
    if (method == "lin") {
      FunctionalForm  = formula(y ~ a - b * x)
    } else if (method == "exp") {
      FunctionalForm  = formula(y ~ a * exp(-b * x))
    } else if (method == "hyp") {
      FunctionalForm  = formula(y ~ 1/(a + b * x))
    } else if (method == "log") {
      FunctionalForm  = formula(y ~ a - b * log(x))
    } else if (method == "pow") {
      FunctionalForm  = formula(y ~ a * x^(-b))
    } else if (method == "exp-pow") {
      FunctionalForm  = formula(y ~ a * exp(-2 * b * sqrt(x)))
    }
  } else if (asym0.5 == T) {
    if (method == "lin") {
      FunctionalForm  = formula(y ~ 1 - 1/(a - b * x + 2))
    } else if (method == "exp") {
      FunctionalForm  = formula(y ~ 1 - 1/(a * exp(-b * x) + 2))
    } else if (method == "hyp") {
      FunctionalForm  = formula(y ~ 1 - 1/(1/(a + b * x) + 2))
    } else if (method == "log") {
      FunctionalForm  = formula(y ~ 1 - 1/(a - b * log(x) + 2))
    } else if (method == "pow") {
      FunctionalForm  = formula(y ~ 1 - 1/(a * x^(-b) + 2))
    } else if (method == "exp-pow") {
      FunctionalForm  = formula(y ~ 1 - 1/(a * exp(-2 * b * sqrt(x)) + 2))
    }
  }
  res = nls(FunctionalForm, start=list(a=abs(rnorm(1)),b=abs(rnorm(1))))
  rsq = sum((predict(res, x)-mean(y))^2)/(sum((y-mean(y))^2))
  
  if (loud == F) return(rsq)
  else if (loud == T) {
    resSum = summary(res)
    params = resSum$coefficients[,1]
    return(list(Rsquared = rsq,
                EstParam = params))
  }
 }


########################################################################################
## predict model values

predictValues = function(support, method, params, asym0.5 = F){
  if (asym0.5 == F){
    if (method == "lin"){
      predicts = params[1] - params[2] * support
    } else if (method == "exp"){
      predicts = params[1] * exp(-params[2] * support)
    } else if (method == "hyp"){
      predicts = 1/(params[1] + params[2] * support)
    } else if (method == "log"){
      predicts = params[1] - params[2] * log(support)
    } else if (method == "pow"){
      predicts = params[1] * support^(-params[2])
    } else if (method == "exp-pow"){
      predicts = params[1] * exp(-2 * params[2] * sqrt(support))
    }
  } else if (asym0.5 == T) {
    if (method == "lin"){
      predicts = 1 - 1/(params[1] - params[2] * support + 2)
    } else if (method == "exp"){
      predicts = 1 - 1/(params[1] * exp(-params[2] * support) + 2)
    } else if (method == "hyp"){
      predicts = 1 - 1/(1/(params[1] + params[2] * support) + 2)
    } else if (method == "log"){
      predicts = 1 - 1/(params[1] - params[2] * log(support) + 2)
    } else if (method == "pow"){
      predicts = 1 - 1/(params[1] * support^(-params[2]) + 2)
    } else if (method == "exp-pow"){
      predicts = 1 - 1/(params[1] * exp(-2 * params[2] * sqrt(support)) + 2)
    }
  }
  return(predicts)
}



