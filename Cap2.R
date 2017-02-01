

set.seed(25)
x <- seq(0, 2, 0.02)
y <- exp(-4*x+4)/ (1+exp(-4*x+4))
x <- sort(x)[1:20]
y <- sort(sample(y,20))
plot(x,y,xlab="x", ylab=quote(p(x)), cex = 0.5, cex.lab = 0.7, cex.axis = 0.4, ylim = c(-0.1, 1.1),las=1)
abline(lm(y~x), col = "red")
abline(h = 1, lty = 2)
abline(h = 0, lty = 2)



# con plogis y pnorm se obtienen las funciones de distribución logística y normal respectivamente
curve(plogis(x),-4,4, cex.axis = 0.5,ylab="p(x)",lwd=1.3, las=1)
curve(pnorm(x),-4,4,add=TRUE,col="red",lty=2,lwd=1.5)
curve(1-exp(-exp(x)),add=TRUE,xlim=c(-4,4),lty=3,col="blue",lwd=2)#cloglog
curve(exp(-exp(-x)),add=TRUE,xlim=c(-4,4),lty=5,col="darkgreen",lwd=2)#loglog
legend("topleft", c("logit","probit","cloglog","loglog"), col=c("black","red","blue","darkgreen"), lty=c(1:3,5), lwd=c(1.1,1.1,2.5,1.3), cex=0.7)



x1<-seq(-10,10,length=17)
x2<-seq(-10,10,length=17)
f <- function(x,y){p <- exp(1+x+y)/(1+exp(1+x+y))}
z <- outer(x1,x2,f)
persp(x1,x2,z,theta = 35, phi = 28, expand = 0.5, col = "lightblue")


