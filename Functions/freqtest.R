freq<-read.csv("C:/Users/Kenji/Documents/STSCI/frequency.csv")

freq<-
  
library(MASS)
x=apply(freq[,11:17],1,sum)
plot(density(x))
beta=mean(x)/var(x)
alpha=mean(x)^2/var(x)
xfit <- 0:max(x)
yfit4=dgamma(xfit,shape=alpha,rate=beta)
lines(xfit,yfit4,col='grey')

par(mfrow=c(3,3))
for (i in 2:10){
  hist(freq[,i],breaks=12)
}

windows()
par(mfrow=c(3,3))
for (j in 11:19){
  hist(freq[,j],breaks=12)
}

windows()
par(mfrow=c(2,2))
for (j in 20:23){
  hist(freq[,j],breaks=12)
}

cms<-colMeans(freq[,2:25])
windows()
barplot(prop.table(cms))

library(mixtools)
for (ii in 8:23) {
  mixmdl = normalmixEM(freq[,ii])
  
  plot(mixmdl,which=2,breaks=12)
  lines(density(freq[,ii]),lty=2, lwd=2)
}

mixmdl = normalmixEM(freq[,12])
plot(mixmdl,which=2)
lines(density(freq[,12]),lty=2, lwd=2)


lambda1 = mixmdl$lambda[1]
lambda2 = mixmdl$lambda[2]
mu1 = mixmdl$mu[1]
mu2 = mixmdl$mu[2]
sigma1 = mixmdl$sigma[1]
sigma2 = mixmdl$sigma[2]
px = lambda1*dnorm(1000,mean=mu1,sd=sigma1) + lambda2*dnorm(1000,mean=mu2,sd=sigma2)
  











