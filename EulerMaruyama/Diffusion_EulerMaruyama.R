# Euler-Maruyama Approximation for Diffusion Model 
# Drawing Figure 3. Tavares,Perona,Rangel(2017)
# Programmed by Hyunwoo Gu 2019.05.21.
########################################################

## Here I assumed the parameters & eye position is given as in Figure 3.
## Ornstein-Uhlenbeck is a generalization, so I used it.
## I have not found the threshold param, delta_t, plus the meaning of "Y" axis
## So some gaps were bridged to implement.

## Model parameters
d = 0.002
r_left  = 2
r_right = 2
theta   = 0.5
sigma   = 0.02

## Simulation parameters
num_sim = 5    ### display five runs
t_init  = 0    ### initial time
t_end   = 2000 ### ending time
N       = 2000 ### delta_t = 1ms assumed
delta_t = (t_end - t_init) / N  ### discretized time
y_init  = 0    ### Y_0

## Simulation variables
ts    = seq(t_init, t_end, by=delta_t) ## series of time t
ys    = vector(length=N)               ## series of Y (initialized)
ys[1] = y_init                         ## initialize Y0

## Functions 
### current eye position (0: left, 1:right)
current_state = function(t) {
  if ((t>450 & t<550) | (t>1250 & t<1350)){  ## intermittent period
    return (Inf)
  } else if ((t>300 & t<500) | (t>1300) ){   ## left
    return (0)
  } else if (t>=500 & t<=1300) {             ## right
    return (1)
  } else {
    return (Inf)                             ## not specified
  }
}

### a(t) in Ornstein-Uhlenbeck : not depend on t here
a  = function(t){
  if (current_state(t) == 0){
    return(d * (r_left- theta*r_right))  # left, then return this drift
  } else if (current_state(t) == 1) {
    return(d * (theta*r_left- r_right))  # right, then return this drift
  } else{
    return(0) # if in-between, then return 0 drift
  }
}

current_state(1)

### b(t) in Ornstein-Uhlenbeck : just 1 here
b  = function(y_n){ return(1) }

### dW in Ornstein-Uhlenbeck : epsilon in paper
dW = function(dt){ return(rnorm(1,mean=0,sd=sqrt(dt))) }


## Plot
plot(NULL, type="n", xlab="Time", ylab="Y", xlim=c(t_init, t_end), ylim=c(-70, 70)) ## NULL plot

### Shade for the states. Apology for poor aesthetics...
abline(v = c(seq(300,450,by=0.1), seq(1350,2000,by=0.1)), col=alpha("purple",0.01))
abline(v = c(seq(500,1250,by=0.1)), col=alpha("yellow",0.01))

### Iterates!
for (iter in 1:num_sim){
  for (i in 2:length(ts)){
    t     = (i-1)*delta_t ## increment of time by dt
    y     = ys[i-1]       ## current y
    ys[i] = y + a(t) * delta_t + b(y) * dW(delta_t) ## generate next observation
  }
  lines(ts, ys, col=iter)
}

