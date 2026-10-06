rm(list=ls())

d=read.csv("C:/Users/sujay/OneDrive/Documents/Dissertation/oil_price_dataset.csv")
d
x=d$oil_price;x
data=x;data
a=matrix(data,ncol=12,byrow=T)

plot(d$oil_price,type="l")
plot(apply(a,2,mean),type="l",main="for each month")
plot(apply(a,1,mean),type="l",main="for each year")


d=ts(data,start=1986,frequency=12)
plot(d)


index=decompose(d,type="multiplicative")$figure;index
plot(index,type="l")

length(d)
deseasonalize=as.vector(d)/rep(index,35);deseasonalize		#deseasonalization
plot(deseasonalize,type="l")

time=1:(12*35)
fit1=fitted(lm(deseasonalize~time))
fit2=fitted(lm(deseasonalize~time+I(time^2)))
summary(lm(deseasonalize~time+I(time^2)))
summary(lm(deseasonalize~time))

data1=ts(fit1,start=c(1986,1),frequency=12)
data2=ts(fit2,start=c(1986,1),frequency=12)

residual_1=deseasonalize/data1;residual_1
residual_2=deseasonalize/data2;residual_2		                     #detrendation
detrend1=ts(residual_1,start=c(1986,1),frequency=12)
detrend2=ts(residual_2,start=c(1986,1),frequency=12)

matplot(cbind(deseasonalize,fit1),type="l")
matplot(cbind(deseasonalize,fit2),type="l")

plot(detrend1,type="l",col="red")              #for linear relation of residual series
plot(detrend2,type="l",col="green")            # for quadratic relation

MAPE1=mean(abs(resid(lm(deseasonalize~time+I(time^2))))/deseasonalize) ;MAPE1*100

MAPE2=mean(abs(resid(lm(deseasonalize~time)))/deseasonalize) ;MAPE2*100



u1=acf(residual_1,type="correlation",plot=T)
u2=acf(residual_2,type="correlation",plot=T)
u1
u2


