model <- function(t, state, parms){
  with(as.list(c(state,parms)),{
    N <- state[1:n]
    S <- sum(N)
    dN <- b*N*(1-S/K) - d*N
    return(list(c(dN)))    
  }) 
} 

mutate <- function(t, state, parms){
  nmut <- rep(0,n+1)
  emut <- parms["mu"]*state      # Expected number of mutants
  nmut[2:(n+1)] <- ifelse(emut>1,emut,ifelse(runif(n)<emut,1,0))
  state <- state + nmut[1:n] - nmut[2:(n+1)]
  if (t < 100) cat("Time",t,":",nmut[1:n],"\n")
  return(state)
}     

n <- 10                        # number of variants with
b <- seq(1,2,length=n)         # increasing birth rates
p <- c(d=0.1,K=5000,mu=0.001)  # other parameters
s <- rep(0,n)                  # set all variables to 0
s[1] <- 1                      # set first to 1
names(s) <- paste("N",seq(1,n),sep="")     # add names
run(1000,ymax=5000)            # run a 1000 time steps

run(1000,ymax=5000,after="state<-mutate(t,state,parms)")
