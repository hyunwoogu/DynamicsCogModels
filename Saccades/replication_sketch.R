################################################################################################ 
# Latency distribution data of DH (read from Fig. 2)
################################################################################################
Time = c(170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300,  310,  320, 330, 340)
HC   = c(0.3, 2,   7,   16,  31,  48,  62,  70,  80,  86,  92,  94,  96,  97.5, 99.1, 99.6, NA, NA)
LC   = c(NA , 0.3, 1.2, 3,   8,   18,  27,  38,  52,  61,  72,  79,  84,  90,   96,   98,  99.1, 99.7)

## x, y axis labels
x_ticks = c(150, 200, 300, 500, 1000)
y_ticks = c(0.1,0.5,1,2,5,10,20,30,40,50,60,70,80,90,95,98,99,99.5,99.9)


################################################################################################
# Latency distribution plot in original scale
################################################################################################
plot(Time, HC, pch=16, main="Latency Distribution of DH", xlab="Sccadic Latency(ms)", ylab="Cumulative probability")
points(Time, LC, pch=1)
legend("bottomright", legend=c("High Contrast", "Low Contrast"),
       pch=c(16,1), box.lty=0)


################################################################################################
# Reciprobit plot of Latency distribution (replication of Fig. 2)
################################################################################################
invTime   = 1/Time ## transfrom x axis into reciprocal scale
HC_probit = qnorm(HC/100); LC_probit = qnorm(LC/100) ## transfrom y axis into probit scale

## High contrast plot (without x/y labels)
plot(invTime, HC_probit, 
     xlim=rev(range(invTime)), pch=16, labels = FALSE,  ## plz ignore warnings... it's due to labels=FALSE here
     main="Latency Distribution of DH", xlab="Sccadic Latency(ms)", ylab="Cumulative probability")

## Low contrast plot
points(invTime, LC_probit, xlim=rev(range(1/Time)), pch=1)
legend("bottomright", legend=c("High Contrast", "Low Contrast"),
       pch=c(16,1), box.lty=0)

## Add median line / labels
abline(h=qnorm(50/100)) # add median line
axis(1, at = 1/x_ticks, labels=x_ticks, tick=T) # add x labels
axis(2, at = qnorm(y_ticks/100), labels = y_ticks, tick=T) # add y labels


################################################################################################ 
# Inhibition function data of DH (read from Fig. 3)
################################################################################################
Delta      = c(0,   50,  90,  130, 170, 210)
HC_inhibit = c(.01, .08, .45, .82, .98, NA)
LC_inhibit = c(NA,  .02, .18, .54, .88, .99)


################################################################################################
# Inhibition function distribution plot (w/o error bars)
################################################################################################
plot(Delta, HC_inhibit, pch=16, 
     main="Inhibition function of DH", xlab=paste(expression(delta), "(ms)"), ylab=expression(P(delta)))
lines(Delta, HC_inhibit)
points(Delta, LC_inhibit, pch=1)
lines(Delta, LC_inhibit)


################################################################################################
# Approximation of original data
################################################################################################

## 
cdf_HC = approxfun(Time, HC/100, method="linear", rule=2)
inv_cdf_HC = function(y){uniroot(function(x){cdf_HC(x)-y},interval=c(150,350))$root}
inv_cdf_HC = Vectorize(inv_cdf_HC)

X = runif(1000,0.01,0.99)
HC_sample = inv_cdf_HC(X)
mean(HC_sample)
plot(density(HC_sample))

polygon(c( x[x>=1250], 1250 ),  c(y[x>=1250],0 ), col="red")


##
cdf_HC_inhibit = approxfun(Delta, HC_inhibit, method="linear", rule=2)
inv_cdf_HC_inhibit = function(y){uniroot(function(x){cdf_HC_inhibit(x)-y},interval=c(0,250))$root}
inv_cdf_HC_inhibit = Vectorize(inv_cdf_HC_inhibit)

cdf_HC_inhibit(150)
inv_cdf_HC_inhibit(0.97)

supp = seq(0.01, 0.97, length=1e4)
plot(supp, inv_cdf_HC_inhibit(supp))
inv_cdf_HC(0.4)

X = runif(1000,0.01,0.97)
HC_inhibit_sample = inv_cdf_HC_inhibit(X)
mean(HC_inhibit_sample)



################################################################################################
# SSRT estimates
################################################################################################

## Method 1. integration

dens_function = approxfun(density(HC_sample)$x, density(HC_sample)$y, method="linear", rule=2)

Delta      = c(0,   50,  90,  130, 170, 210)
HC_inhibit = c(.01, .08, .45, .82, .98, NA)
LC_inhibit = c(NA,  .02, .18, .54, .88, .99)

dens_fun = dens_function
obj = HC_inhibit
SSRT_estimate_loss_func = function(SSRT_hat, dens_fun, obj){
  integrated_val = vector(length=6)
  for (i in 1:6){
    lower_lim = 0
    upper_lim = Delta[i] + SSRT_hat 
    integrated_val[i] = integrate(dens_fun, lower_lim, upper_lim)$value
  }
  return(sum((obj-integrated_val)^2, na.rm=T))  ## SSE
}

optimize(SSRT_estimate_loss_func, interval=c(50,200), dens_fun=dens_function, obj=HC_inhibit)

Delta      = c(0,   50,  90,  130, 170, 210)
HC_inhibit = c(.01, .08, .45, .82, .98, NA)
LC_inhibit = c(NA,  .02, .18, .54, .88, .99)



## Method 2. difference of means
mean(HC_sample) - mean(HC_inhibit_sample) 
mean(LC_sample) - mean(LC_inhibit_sample)


################################################################################################
# Approximation of original data
################################################################################################

## read from Table 3
mu_go_hc      = 6.11
mu_stop_hc    = 13.8
sigma_go_hc   = 1.05
sigma_stop_hc = 3.0
mu_go_lc      = 5.27
mu_stop_lc    = 14.8
sigma_go_lc   = 0.88
sigma_stop_lc = 3.2



##
tau = 60
mu = 6
sigma = 1
theta = 1
trials = 2048


simul_control_trial = function(par, tau=60, theta=1, trials=2048, iter_max=200){
  mu = par[1]; sigma = par[2]
  activ_level = vector(length=trials)
  rt_result   = vector(length=trials)
  for (time in 1:iter_max){
    activ_level = activ_level + 0.01 * rnorm(trials, mean=mu, sd=sigma) ## accumulation of act. level
    rt_result[activ_level>theta] = tau + 10*time  ## unit : ms
    activ_level[activ_level>theta] = -Inf
  }
  return(rt_result)
}


##
Kolmo_Smir_loss_between_simul_and_observed = function(par, observed){
  simul_res = simul_control_trial(par) ## save the simulated data from given parameters
  ecdf_simul = ecdf(simul_res) ## empricial cdf from the simulated result
  sum(abs(ecdf_simul(Time)-observed/100), na.rm=T)
}

Kolmo_Smir_loss_between_simul_and_observed(c(6,1), HC)

optim(par=c(6,1), Kolmo_Smir_loss_between_simul_and_observed, observed=LC)




simul_res = simul_control_trial(c(6,1)) ## 
plot(density(simul_res, bw=4))
plot(ecdf(simul_res), verticals=TRUE, do.points=T)
ecdf_simul = ecdf(simul_res)


invTime   = 1/Time ## transfrom x axis into reciprocal scale
HC_probit = qnorm(HC/100); LC_probit = qnorm(LC/100) ## transfrom y axis into probit scale

simul_probit = qnorm(ecdf_simul(Time))
plot(invTime, simul_probit, 
     xlim=rev(range(invTime)), pch=16, labels = FALSE,  ## plz ignore warnings... it's due to labels=FALSE here
     main="Latency Distribution of DH", xlab="Sccadic Latency(ms)", ylab="Cumulative probability")
abline(h=qnorm(50/100)) # add median line
axis(1, at = 1/x_ticks, labels=x_ticks, tick=T) # add x labels
axis(2, at = qnorm(y_ticks/100), labels = y_ticks, tick=T) # add y labels

## High contrast plot (without x/y labels)
plot(invTime, HC_probit, 
     xlim=rev(range(invTime)), pch=16, labels = FALSE,  ## plz ignore warnings... it's due to labels=FALSE here
     main="Latency Distribution of DH", xlab="Sccadic Latency(ms)", ylab="Cumulative probability")

## Low contrast plot
points(invTime, LC_probit, xlim=rev(range(1/Time)), pch=1)
legend("bottomright", legend=c("High Contrast", "Low Contrast"),
       pch=c(16,1), box.lty=0)

## Add median line / labels
abline(h=qnorm(50/100)) # add median line
axis(1, at = 1/x_ticks, labels=x_ticks, tick=T) # add x labels
axis(2, at = qnorm(y_ticks/100), labels = y_ticks, tick=T) # add y labels

