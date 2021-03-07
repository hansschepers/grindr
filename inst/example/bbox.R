library(Grind)
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    
    dR <- r*R*(1 - R/K) - a*R*N
    dN <- c*a*R*N - delta*N
    
    return(list(c(dR, dN)))  
  }) 
}

p <- c(r=1,K=1,a=1,c=1,delta=0.5)
s <- c(R=1,N=0.01)

mgp <- c(3, 1, 0)                  # Default
mar <- c(5.1, 4.1, 4.1, 2.1)       # Default
opar <- par(no.readonly=TRUE)      # Keep default par() in opar

plane(eps=-0.001,main="Title",sub="Subtitle")
mtext("Bottom",1);mtext("Left",2);mtext("Top",3);mtext("Right",4)
par(mar=c(3.6,2.6,1.6,0.2),mgp=c(1.5,0.5,0))     # mar=c(B,L,T,R)
plane(eps=-0.001,main="Title",sub="Subtitle")
par(mar=c(2.6,2.6,1.6,0.2),mgp=c(1.5,0.5,0)) 
plane(eps=-0.001,main="Title",sub="")
par(mar=c(2.6,2.6,0.2,0.2),mgp=c(1.5,0.5,0)) 
plane(eps=-0.001,main="",sub="")
par(mar=c(0.1,0.1,0.1,0.1),xaxt="n",yaxt="n",ann=F) 
plane(eps=-0.001)
run()
par(mar=c(0.1,0.1,0.1,0.1),xaxt="n",yaxt="n",ann=F,xaxs="i",yaxs="i") 
plane(eps=-0.001)
run()

pdf("plane.pdf")
par(mar=c(0.1,0.1,0.1,0.1),xaxt="n",yaxt="n",ann=F,xaxs="i",yaxs="i") 
plane(eps=-0.001)
dev.off()

pdf("run.pdf")
par(mar=c(0.1,0.1,0.1,0.1),xaxt="n",yaxt="n",ann=F,xaxs="i",yaxs="i") 
run()
dev.off()

par(opar)                          # Reset parameters
run()
