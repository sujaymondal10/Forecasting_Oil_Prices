rm(list=ls())

d=read.csv("C:/Users/sujay/OneDrive/Documents/Dissertation/oil_price_dataset.csv")
d
x=d$oil_price;x

data=ts(x,start=1986,frequency=12)
plot(data)

#---------------------------------------------------------------
#ELIMINATING TREND

ma = 0
n = nrow(d) - 11; n
for(i in 1:n)
{
   ma[i] = mean(x[i:(i+11)])
}
length(ma)
ma

MA = 0
for(i in 1:(n-1))
{
   MA[i] = mean(ma[i:(i+1)])
}
length(MA)   # Final centered MA values

plot(MA,type="l")

dtrnd = x[7:(nrow(d)-6)]-MA            #TAKING ADDITIVE MODEL
plot(dtrnd,type="l") 

#-----------------------------------------------------------------------
#ELIMINATING SEASONAL VARIATION

data1=ts(dtrnd,start=1986,frequency=12)

index=decompose(data1,type="additive")$figure
sum(index)                                           #from july to june
index_1=c(index[c(7:12)],index[c(1:6)])            #from jan to dec
plot(index_1,type="l")
index_1

d2 = matrix(c(rep(0,6), dtrnd, rep(0,6)),
            byrow = T, ncol = 12); d2
USI = apply(d2, 2, sum)/34; USI
S = sum(USI)/12; # adjustment factor
S
ASI = USI - S; ASI
sum(ASI)
plot(ASI,type="l")                      #index_1 and ASI are same here

ASI_1=c(ASI[c(7:12)],ASI[c(1:6)])            #from july to june


length(d)
deseasonalize=as.vector(dtrnd)-rep(ASI_1,34);deseasonalize		#deseasonalization
plot(deseasonalize,type="l")                               #from july to june


#------------------------------------------------------
#CYCLICAL VARIATION

t1=1:408
n=12*34
mu=1:408
A=array(0)
B=array(0)
for(i in 1:length(mu))
{
A[i]=(2/n)*sum(deseasonalize*cos(2*pi*t1/mu[i]))
B[i]=(2/n)*sum(deseasonalize*sin(2*pi*t1/mu[i]))
}
S.mu=(A^2)+(B^2)
plot(mu,S.mu,type="h")
#lambda=mu[which(S.mu==max(S.mu))]
lambda=mu[which.max(S.mu)]
lambda


u_adj=deseasonalize[1:15]
t2=1:15                     #lambda=15
A0_hat=mean(u_adj);A0_hat
A_hat=2*mean(u_adj*cos(2*pi*t2/lambda));A_hat
B_hat=2*mean(u_adj*sin(2*pi*t2/lambda));B_hat
fitted=A0_hat+A_hat*cos(2*pi*t2/lambda)+B_hat*sin(2*pi*t2/lambda);fitted
matplot(cbind(u_adj,fitted),type="l")
residual=deseasonalize[c(-405,-407,-408)]-rep(fitted,27)     #from 1 to 405 month

plot(residual,type="l")

#---------------------------------------------------------------------------
#CYCLICAL_2

k=as.array(deseasonalize)
k
 
sum1=sum2=0
c=array(dim=1)
d=array(dim=1)
C=array(dim=1)
D=array(dim=1)
Rmu=array(dim=1)
for (i in 1:408)            #LAMBDA
{
 for (t in 1:408)
 { 
 c[t]=k[t]*cos(2*3.147*t/i)
 sum1=sum1+c[t]
 d[t]=k[t]*sin(2*3.147*t/i)
 sum2=sum2+d[t]
 }
 C[i]= ( 2/ 408)* sum1
 D[i] = ( 2/ 408)*sum2
 Rmu[i]=(C[i]^2) + (D[i]^2)
}
plot(Rmu,type="l")
x=as.vector(Rmu)
max=max(Rmu)
lambda1=which (x==max, T) #true period
lambda1
C0=mean(k)
C0
C1=C[lambda1]
C1
D1=D[lambda1]
Ut=array(dim=1)
for (t1 in 1:408)
{
 Ut[t1]=C0+(C1*cos(2*3.14*t1/lambda1)) + (D1*sin(2*3.14*t1/lambda1))
}
plot(Ut,type="l")
It=deseasonalize -Ut
It
plot(It,type="l")

#_________________________________________________________________-
#MANN KENDALL TEST
library(trend)
mat=matrix(x,ncol=12,byrow=T)
mann=apply(mat,1,sum)
mk.test(mann)                    #H0 is rejected at 0.05 L.O.S.

