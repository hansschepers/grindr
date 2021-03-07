# Chaos in a three-species food chain: 
# Hastings & Powell (1991) Ecology 72. 896-903.

# This illustrates:
# 3-dimensional nullclines and chaotic trajectories
# Takens reconstruction by shifting the numerical output
# Effect of noise on a parameter or a variable on the attractor
# Bifurcation diagram using continue() and by following 
# a limit cycle by tracking its minimum and maximum values.

# Source both grind.R and cube.R! 

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dR <- R*(1 - R/K) - c1*N*R/(1+b1*R)
    dN <- -aN*N + c1*N*R/(1+b1*R) - c2*Z*N/(1+b2*N)
    dZ <- -aT*Z + c2*Z*N/(1+b2*N)
    return(list(c(dR, dN, dZ)))  
  }) 
}  

opar <- par()     # Save graphics parameters
par(mar=c(3.6,2.6,1.6,0.2),mgp=c(1.5,0.5,0))

p <- c(b1=6,b2=2,c1=5,c2=0.1,aN=0.4,aT=0.01,K=1)
s <- c(R=1,N=0.1,Z=0.01)
plane(eps=-.01)
plane(eps=-.01,show=names(s),zero=F)
f <- newton(c(R=0.1,N=0.23,Z=0),plot=T)
run(50,tstep=0.1,traject=TRUE)

s <- c(R=1,N=0.1,Z=0.1)
f <- run(1e4)       # Burn in
cube(x=1,y=3,z=2,ymax=6,eps=1e-9)
run3d(x=1,y=3,z=2,tmax=5000,tstep=0.2,state=f,add=TRUE)
plotdev(theta=10,phi=20)

# Plot same trajectory using the rgl library
library(rgl)
data <- run(tmax=5000,tstep=0.2,state=f,timeplot=FALSE,table=TRUE)
plot3d(x=data$R,y=data$N,z=data$Z,xlab="R",ylab="N",zlab="Z",type="l")

# Make a Takens reconstruction:
data <- run(tmax=5000,state=f,table=TRUE)
plot(data$R[1:4999],data$R[2:5000],pch=".")

# Noise on a parameter, run and a Takens reconstruction:
noiseP="parms[\"K\"]<-abs(rnorm(1,1,0.1))"
data <- run(1e4,state=f,table=TRUE,after=noiseP)
plot(data$R[5001:9999],data$R[5002:1e4],pch=".")

# Noise on a variable, run and a Takens reconstruction:
noiseS="if(runif(1)<0.1)state[3]=state[3]+abs(rnorm(1,0,0.1))"
data <- run(1e4,state=f,table=TRUE,after=noiseS)
plot(data$R[5001:9999],data$R[5002:1e4],pch=".")

# Make a bifurcation diagram
p["b1"] <- 1
s <- c(R=1,N=0.1,Z=0.1)
f <- run(1e4)    # Burn in
continue(newton(f),x="b1",xmin=0,xmax=4,y="R",atol=1e-9,rtol=1e-9)
p["b1"] <- 2.14  # Start at Hopf bifurcation
burnin <- 1e4; tmax <- 500
for (b1 in seq(2.14,4,0.01)) {
  p["b1"] <- b1
  f <- run(burnin,state=f,timeplot=FALSE)
  data <- run(tmax,tstep=0.2,state=f,table=TRUE,timeplot=FALSE)
  nr <- nrow(data)
  datamin <- data[(1+which(data$R[2:(nr-1)]<data$R[1:(nr-2)] & data$R[2:(nr-1)]<data$R[3:nr])),]
  datamax <- data[(1+which(data$R[2:(nr-1)]>data$R[1:(nr-2)] & data$R[2:(nr-1)]>data$R[3:nr])),]
  points(rep(b1,nrow(datamin)),datamin$R,pch=".")
  points(rep(b1,nrow(datamax)),datamax$R,pch=".")
}

