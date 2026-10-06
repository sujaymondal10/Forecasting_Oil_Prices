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

dtrnd = (x[7:(nrow(d)-6)]/MA)*100            #TAKING MULTIPLICATIVE MODEL
plot(dtrnd,type="l") 

#-----------------------------------------------------------------------
#ELIMINATING SEASONAL VARIATION

data1=ts(dtrnd,start=1986,frequency=12)

index=decompose(data1,type="multiplicative")$figure
sum(index)                                           #from july to june
index_1=c(index[c(7:12)],index[c(1:6)])            #from jan to dec
plot(index_1,type="l")
index_1

d2 = matrix(c(rep(0,6), dtrnd, rep(0,6)),
            byrow = T, ncol = 12); d2
USI = apply(d2, 2, sum)/34; USI
S = sum(USI); # adjustment factor
S
ASI = USI*1200/sum(USI); ASI
sum(ASI)
plot(ASI,type="l")                      #index_1 and ASI are same here

ASI_1=c(ASI[c(7:12)],ASI[c(1:6)])            #from july to june


length(d)
deseasonalize=as.vector(dtrnd)/rep(ASI_1,34);deseasonalize		#deseasonalization
plot(deseasonalize,type="l")                               #from july to june



#---------------------------------------------------------------------------
#CYCLICAL

k=as.array(deseasonalize)
k
 
sum1=sum2=0
a=array(dim=1)
b=array(dim=1)
A=array(dim=1)
B=array(dim=1)
Rmu=array(dim=1)
for (i in 1:100)            #LAMBDA
{
 for (t in 1:408)
 { 
 a[t]=k[t]*cos(2*3.147*t/i)
 sum1=sum1+a[t]
 b[t]=k[t]*sin(2*3.147*t/i)
 sum2=sum2+b[t]
 }
 A[i]= ( 2/ 408)* sum1
 B[i] = ( 2/ 408)*sum2
 Rmu[i]=(A[i]^2) + (B[i]^2)
}
plot(Rmu,type="l")
x=as.vector(Rmu)
max=max(Rmu)
lambda=which (x==max, T) #true period
lambda
A0=mean(k)
A0
A1=A[lambda]
A1
B1=B[lambda]
Ut=array(dim=1)
for (t1 in 1:408)
{
 Ut[t1]=A0+(A1*cos(2*3.14*t1/lambda)) + (B1*sin(2*3.14*t1/lambda))
}
plot(Ut,type="l")
It=deseasonalize /Ut

plot(It,type="l")





#MANN KENDALL TEST
library(trend)
mat=matrix(x,ncol=12,byrow=T)
mann=apply(mat,1,sum)
mk.test(mann)                    #H0 is rejected at 0.05 L.O.S.

