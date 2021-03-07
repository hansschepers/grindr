# http://tbb.bio.uu.nl/rdb/practicals/grindR/separatrix.R
# This is the operon model again

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    
    R = 1/(1+A^n)               # Repressor
    dA = M*L - delta*A - v*M*A  # Allolactose
    dM = c0 + c*(1-R) - d*M     # mRNA
    
    return(list(c(dA, dM)))  
  }) 
}

p <- c(L=1,c=1,c0=0.05,d=1,delta=0.2,n=5,v=0.25)
s <- c(A=0,M=0)

# Define backwards ODEs:
back <- function(t, state, parms) return(list(-1*unlist(model(t,state,parms))))
# Stop when one of the variables becomes negative:
after <- "if(min(state)<0)break"
plane(tmax=10,tstep=0.1,xmax=3,ymax=1.1,portrait=T)
f <- newton(c(A=0.8,M=0.2),plot=T,vector=T)
# Copy the first eigenvector into v:
v <- c(-0.5569487,0.8305469)
# Step away from saddle point along the eigenvector and run
fp <- f + 0.01*v
run(10,tstep=0.01,state=fp,odes=back,traject=T,lwd=3,after=after,col=3,pch=NA_integer_)
# Step away from saddle point along the eigenvector and run 
fp <- f - 0.01*v
run(10,tstep=0.01,state=fp,odes=back,traject=T,lwd=3,after=after,col=3,pch=NA_integer_)
# Note that the pch=NA_integer_ suppresses the bullet at the start of the trajectory
