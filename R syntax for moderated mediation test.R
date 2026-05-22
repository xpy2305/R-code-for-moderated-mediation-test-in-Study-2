#1.	Leader empathy x follower workplace anxiety signaling on follower perceived acceptability of norm violation via leader victim-supporting norm violation 

#moderated mediation
require(MASS)
a=0.10 # a path
b=0.39 # b path
c=1.17  #  a1
wsd=.4457 
a2=0.0181944  # the variance of a estimation, read in tech3
b2=0.0128227
c2=0.0638506
ab=0.00326817 # the covariance of a estimation and b estimation, read in tech 3
ac=0.0130102
bc=0.00335777
rep=20000
conf=95
pest=c(a,b,c)
acov <- matrix(c(
  a2,ab,ac,
  ab,b2,bc, 
  ac,bc,c2
),3,3)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
MoMe <- mcmc[,3]*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(MoMe,low)
UL=quantile(MoMe,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
hist(MoMe,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')

#high & low level of moderator
require(MASS)
a=0.10 # a path
b=0.39 # b path
c=1.17  #  a1
wsd=.4457 
a2=0.0181944  
b2=0.0128227
c2=0.0638506
ab=0.00326817
ac=0.0130102
bc=0.00335777
rep=20000
conf=95
pest=c(a,b,c)
acov <- matrix(c(
  a2,ab,ac,
  ab,b2,bc, 
  ac,bc,c2
),3,3)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
amvec<-(wsd)*mcmc[,3]+mcmc[,1]  #for low, change into (-wsd)  
MoMeH <- amvec*mcmc[,2]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(MoMeH,low)
UL=quantile(MoMeH,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
hist(MoMeH,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')

#difference between high and low levels
require(MASS)
a=0.10 # a path
b=0.39 # b path
c=1.17  #  a1
wsd=.4457 
a2=0.0181944
b2=0.0128227
c2=0.0638506
ab=0.00326817
ac=0.0130102
bc=0.00335777
rep=20000 
conf=95
pest=c(a,b,c)
acov <- matrix(c(
  a2,ab,ac, 
  ab,b2,bc, 
  ac,bc,c2
),3,3)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)  
ahmvec<-(wsd)*mcmc[,3]+mcmc[,1] #for low, change into (-wsd)
almvec<-(-wsd)*mcmc[,3]+mcmc[,1]
MoMeH <- ahmvec*mcmc[,2]
MoMeL<-almvec*mcmc[,2]
d=MoMeH-MoMeL
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(d,low)
UL=quantile(d,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
hist(d,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')

#2.	Leader empathy x follower workplace anxiety signaling on observers' deviance via leader norm-violating behavior and observers' perceived acceptability of norm violation
#moderated mediation
require(MASS)
a=0.10 # a path
b=0.39 # b path
c=0.16#c path
d=1.17  #  a1
wsd=.4457
a2= 0.0181944 
b2= 0.0128539
c2= 0.00380647
d2= 0.0638506
ab= 0.00330819
ac= 0.00000661949
ad= 0.0130102
bc= 0.000544500
bd=0.00334011
cd=0.000366633
rep=20000
conf=95
pest=c(a,b,c,d)
acov <- matrix(c( 
  a2,ab,ac,ad,
  ab,b2,bc,bd,
  ac,bc,c2,cd,
  ad,bd,cd,d2
),4,4)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
MoMe <- mcmc[,2]* mcmc[,3]* mcmc[,4]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(MoMe,low)
UL=quantile(MoMe,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
hist(MoMe,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')

#high & low level of the moderator 
require(MASS)
a=0.10 # a path
b=0.39 # b path
c=0.16#c path
d=1.17  #  a1
wsd=.4457 
a2= 0.0181944
b2= 0.0128539
c2= 0.00380647
d2= 0.0638506
ab= 0.00330819
ac= 0.00000661949
ad= 0.0130102
bc= 0.000544500
bd=0.00334011
cd=0.000366633
rep=20000
conf=95
pest=c(a,b,c,d)
acov <- matrix(c(
  a2,ab,ac,ad,
  ab,b2,bc, bd,
  ac,bc,c2, cd,
  ad,bd,cd,d2
),4,4)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
amvec<-(wsd)*mcmc[,4]+mcmc[,1]  #for low, change into (-wsd)  
MoMeH <- amvec*mcmc[,2] *mcmc[,3]
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(MoMeH,low)
UL=quantile(MoMeH,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
hist(MoMeH,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')

#difference between high and low levels
require(MASS)
a=0.10 # a path
b=0.39 # b path
c=0.16#c path
d=1.17  #  a1
wsd=.4457
a2= 0.0181944
b2= 0.0128539
c2= 0.00380647
d2= 0.0638506
ab= 0.00330819
ac= 0.00000661949
ad= 0.0130102
bc= 0.000544500
bd=0.00334011
cd=0.000366633
rep=20000
conf=95
pest=c(a,b,c,d)
acov <- matrix(c(
  a2,ab,ac,ad,
  ab,b2,bc, bd,
  ac,bc,c2, cd,
  ad,bd,cd,d2
),4,4)
mcmc <- mvrnorm(rep,pest,acov,empirical=FALSE)
ahmvec<-(wsd)*mcmc[,4]+mcmc[,1]  #for low, change into (-wsd)  
almvec<-(-wsd)*mcmc[,4]+mcmc[,1]
abh <- ahmvec*mcmc[,2] *mcmc[,3]
abl<-almvec*mcmc[,2] *mcmc[,3]
d=abh-abl
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(d,low)
UL=quantile(d,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
hist(d,breaks='FD',col='skyblue',xlab=paste(conf,'% Confidence Interval ','LL',LL4,'  UL',UL4),
     main='Distribution of Indirect Effect')

