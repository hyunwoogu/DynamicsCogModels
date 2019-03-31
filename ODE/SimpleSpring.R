########################################################################################
########################### Dynamics and Cognitive Models ##############################
############################# Programmed by Hyunwoo Gu #################################
################################### 2019. 4. 2. ########################################
########################################################################################

library(pracma) # for MATLAB-like functions (meshgrid(), quiver())

########################################################################################

# meshgrid()

## Say we want to mark "mesh" on the 2D plane [1:10] x [1:10]
(X = seq(1,10,by=1))
(Y = seq(1,10,by=1))
plot(X, Y) ## which only gives corresponding matches


########################################################################################

## We use meshgrid() to generate every possible match of coordinates
mesh = meshgrid(X, Y)
mesh$X; mesh$Y
plot(mesh$X, mesh$Y)

########################################################################################

## meshgrid() is often used to draw 3D plots (just in case you are interested...)

# library(lattice)
# X = seq(-10,10,length=30)
# Y = seq(-10,10,length=30)
# mesh = meshgrid(X,Y)
# example_function = function(x,y)return (x^2 + y^2)
# wireframe(example_function(mesh$X, mesh$Y))


########################################################################################

# quiver()
## Say we want to draw an arrow from (1,1) to (2,2)
plot(NULL, xlim=c(-1,5), ylim=c(-1,5))  # base null plot
quiver(1,1,            ## from  (1,1)
       1,1, scale = 1) ## go by (1,1) : vector sum

########################################################################################

## we can also draw multiple arrows in one time
plot(NULL, xlim=c(-1,5), ylim=c(-1,5))  # base null plot
quiver(c(0,1),c(0,2),             ## from (0,0) and (1,2)
       c(1,3),c(1,1), scale = 1)  ## go by (1,1) and (3,1)


########################################################################################

# phase portrait
m = 1; k = 1 ## for simplicity
mesh = meshgrid(seq(-0.5, 0.5, by=0.05), seq(-0.5, 0.5, by=0.05)) ## generate mesh

########################################################################################

## we have m (xdotdot) + k x = 0
## Let  X = x     (X axis)
##      Y = xdot  (Y axis)
## Then Xdot = Y
##      Ydot = (Xdotdot = xdotdot) = - (k/m) X
## This is a system of DEs
xdot = mesh$Y 
ydot = - (k/m) * mesh$X

########################################################################################

## Given (X,Y), we calculate (Xdot,Ydot)
## This relationship is plotted, from (X,Y) by (Xdot,Ydot)
plot(NULL,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
quiver(mesh$X,mesh$Y,xdot,ydot)


########################################################################################

# Simulate

## Set initial condition
X0 = 0.2   ## x(0)
Y0 = -0.1  ## xdot(0)
points(X0, Y0, col="red", pch=19, cex=1.5)

########################################################################################

## As time goes by...
new_x = X0; new_y = Y0
step_size = 0.07  ## two simulate derivatives...(but not arbitrarily small, so it fails to meet the start point)
for (t in 1:80){
  temp_x = new_x; temp_y = new_y
  new_x = temp_x + temp_y * step_size
  new_y = temp_y - ((k/m) * temp_x) * step_size
  points(new_x, new_y, col="red", pch=19)
  Sys.sleep(0.1) 
}

## You can check animation for this simulation
## https://physicslens.com/velocity-displacement-graph-of-a-simple-harmonic-oscillator-animation/
