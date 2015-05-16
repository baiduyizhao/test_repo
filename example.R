set.seed(250)
library("ecp")
period1 <- rnorm(100)
period2 <- rnorm(100, 0, 3)
period3 <- rnorm(100, 2, 1)
period4 <- rnorm(100, 2, 4)
Xnorm <- matrix(c(period1, period2, period3, period4), ncol = 1)
output1 <- e.divisive(Xnorm, R = 499, alpha = 1)
output2 <- e.divisive(Xnorm, R = 499, alpha = 2)
output2$estimates

ts.plot(Xnorm, ylab = "Value",main = "Change in a Univariate Gaussian Sequence")
abline(v = c(101, 201, 301), col = "blue")
abline(v = output1$estimates[c(-1, -5)], col = "red", lty = 2)
data("ACGH", package = "ecp")
acghData <- ACGH$data
set.seed(100)
x1 = matrix(c(rnorm(100),rnorm(100,3),rnorm(100,0,2)))
y1 = e.divisive(X=x1,sig.lvl=0.05,R=199,k=NULL,min.size=30,alpha=1)
x2 = rbind(MASS::mvrnorm(100,c(0,0),diag(2)),MASS::mvrnorm(100,c(2,2),diag(2)))


## multivariate
 set.seed(200)
 library("mvtnorm")
mu <- rep(0, 3)
covA <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
 covB <- matrix(c(1, 0.9, 0.9, 0.9, 1, 0.9, 0.9, 0.9, 1), 3, 3)
period1 <- rmvnorm(250, mu, covA)
period2 <- rmvnorm(250, mu, covB)
period3 <- rmvnorm(250, mu, covA)
Xcov <- rbind(period1, period2, period3)
DivOutput <- e.divisive(Xcov, R = 499, alpha = 1)
DivOutput$estimates

## plot multiple y axes in  one plot

# creat dataset
time<-seq(7000,3400,-200)
pop<-c(200,400,450,500,300,100,400,700,830,1200,400,350,200,700,370,800,200,100,120)
grp<-c(2,5,8,3,2,2,4,7,9,4,4,2,2,7,5,12,5,4,4)
med<-c(1.2,1.3,1.2,0.9,2.1,1.4,2.9,3.4,2.1,1.1,1.2,1.5,1.2,0.9,0.5,3.3,2.2,1.1,1.2)
#Define Margins. The trick is to use give as much space possible on the left margin (second value)
par(mar=c(5, 12, 4, 4) + 0.1) # Leave space for z axis
#Plot the first time series. Notice that you don't have to draw the axis nor the labels

plot(time, pop, axes=F, ylim=c(0,max(pop)), xlab="", ylab="",type="l",col="black", main="",xlim=c(7000,3400))
points(time,pop,pch=20,col="black")
axis(2, ylim=c(0,max(pop)),col="black",lwd=2)
mtext(2,text="Population",line=2)
#Plot the second time series. The command par(new=T) is handy here. If you just need to plot two timeseries, you could also use the right vertical axis as well. In that case you have to substitute "2" with "4" in the functions axis() and mtext(). Notice that in both functions lines is increased so that the new axis and its label is placed to the left of the first one. You don't need to increase the value if you use the right vertical axis.
par(new=T)
plot(time, med, axes=F, ylim=c(0,max(med)), xlab="", ylab="", 
     type="l",lty=2, main="",xlim=c(7000,3400),lwd=2)
axis(4, ylim=c(0,max(med)),lwd=2,line=3.5)
points(time, med,pch=20)
mtext(4,text="Median Group Size",line=5.5)

arrows3D(x0 = runif(10), y0 = runif(10), z0 = runif(10),
         x1 = runif(10), y1 = runif(10), z1 = runif(10),
         colvar = 1:10, code = 1:3, main = "arrows3D", colkey = FALSE)

