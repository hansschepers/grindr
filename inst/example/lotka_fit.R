#http://tbb.bio.uu.nl/rdb/practicals/grindR/lotka_fit.R
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    
    dR <- r*R*(1 - R/K) - a*R*N
    dN <- c*a*R*N - delta*N
    
    return(list(c(dR, dN)))  
  }) 
}

p <- c(r=1,K=1,a=1,c=1,delta=0.5)
s <- c(R=1,N=0.01)
data <- run(20,table=T)        # Make a data set

s <- s*abs(rnorm(2,1,0.1));s   # Random guess for initial condition
p <- p*abs(rnorm(5,1,0.1));p   # Random guess for parameters
f <- fit()                     # Fit data with all 7 parameters free
summary(f)                     # Check confidence ranges, etcetera
p <- f$par[3:7];p              # Store estimates in p

p <- p*abs(rnorm(5,1,0.1));p   # Another random guess for the parameters
w <- c(names(s),names(p))      # w provides the names of free parameters
f <- fit(data,free=w)          # Fit the data again
f <- fit(data,initial=T,free=names(p))   # Take initial condition from data

dataR <- data; dataR$N <- NULL # Make two data sets one with R,
dataN <- data; dataN$R <- NULL # and the other with N,
f <- fit(list(dataR,dataN))    # which gives the same result

p <- c(r=1,K=1,a=1,c=1,delta=0.5)  # Start again with same parameters
p["K"] <- 0.75                     # Change K,
s <- c(R=0.5,N=0.05)               # and the initial condition,
data2 <- run(25,table=T)           # and make a new data set
s <- c(R=0.75,N=0.02)              # An "average" guess for the 2 initial conditions
par(mfrow=c(1,2))                  # Show two panels next to each other   
f <- fit(list(data,data2),differ=c("R","N","K"),main=c("A","B"))
f$par                              # Show parameters only

# Provide individual initial guesses as a list:
differ <- list(R=c(0.9,0.55),N=c(0.02,0.04),K=c(1.1,0.7))
f <- fit(list(data,data2),differ=differ,main=c("A","B"))

# Provide fixed parameters as a list:
fixed <- list(R=c(1,0.5),N=c(0.01,0.05))
differ <- "K"                              # one unknown parameter (K)
free <- names(p)[-2];free                  # and four shared unknown parameters
f <- fit(list(data,data2),free=free,differ=differ,fixed=fixed,main=c("A","B"))

# The latter is identical to taking the initial condition from the data:
f <- fit(list(data,data2),free=free,differ=differ,initial=T,main=c("A","B"))

# Finally perform a 100 bootstrap simulations:
fit(list(data,data2),free=free,differ=differ,fixed=fixed,main=c("A","B"),bootstrap=100)$par
par(mfrow=c(1,1)) 

