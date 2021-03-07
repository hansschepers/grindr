#Fitting functional response data to the Monod function:

data <- data.frame(x=c(0, 28, 55, 83, 110, 138, 225, 375),
                   y=c(0, 0.053,0.06,0.112,0.105,0.099,0.122,0.125))
plot(data)

#Using nls():
model0 <- function(x,m=0.1,h=100) return(m*x/(h+x))
curve(model0(x),from=0,to=400,lwd=2,add=T)
f <- nls(y ~ model0(x,m,h),data=data,start=c(m=0.1,h=100))
p <- coefficients(f);p
curve(model0(x,m=p[1],h=p[2]),from=0,to=400,col="blue",add=T)

#Now do the same in grind:
names(data)[1] <- "time"
model <- function(x,state,parms) with(as.list(parms),return(m*x/(x+h))) 
p <- c(m=0.1,h=100)
s <- c(y=0)           # state just provides the name of the second column 
f <- fit(data,solution=T,free=c("m","h"),legend=F)$par 
