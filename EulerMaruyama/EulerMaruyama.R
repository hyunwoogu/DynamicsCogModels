# Euler-Maruyama Approximation
# Codes in https://en.wikipedia.org/wiki/Euler%E2%80%93Maruyama_method
# Programmed by Hyunwoo Gu 2019.05.21.
########################################################

## Model parameters
theta   = 0.1
mu      = 1.5  
sigma   = 0.06

## Simulation parameters
num_sim = 5    ### display five runs
t_init  = 3    ### initial time
t_end   = 7    ### ending time
N       = 1000 ### 1000 grid points
delta_t = (t_end - t_init) / N  ### discretized time
y_init  = 0    ### Y_0

## Simulation variables
ts    = seq(t_init, t_end, by=delta_t) ## series of time t
ys    = vector(length=N)               ## series of Y (initialized)
ys[1] = y_init                         ## initialize Y0

## Function definitions

### Deterministic
a  = function(y_n){ return(theta * (mu - y_n)) }
b  = function(y_n){ return(sigma) } ## not depend on y_n 

### Stochastic
dW = function(dt){ return(rnorm(1,mean=0,sd=sqrt(dt))) }

## Simultation
plot(NULL, type="n", xlab="Time", ylab="Y", xlim=c(t_init, t_end), ylim=c(0, mu+.1)) ## NULL plot

for (iter in 1:num_sim){
  for (i in 2:length(ts)){
    t     = (i-1)*delta_t ## increment of time by dt
    y     = ys[i-1]       ## current y
    ys[i] = y + a(y) * delta_t + b(y) * dW(delta_t) ## generate next observation
  }
  lines(ts, ys, col=iter)
}