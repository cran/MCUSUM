mcusum <-
function(x,n){     #Perform the MCUSUM Control Chart according to Crosier(1988)
if(class(x)!="matrix")(cat("x must be a matrix           "))
k<-0.5
h<-5.5

a<-dim(x)
y<-matrix(0,a[1]/n,a[2])
means<-matrix(0,ncol=ncol(y),nrow=1)
dif<-matrix(0,ncol=ncol(y),nrow=nrow(y))
s<-matrix(0,ncol=ncol(y),nrow=nrow(y))
ci<-matrix(0,ncol=1,nrow=nrow(y))
t2<-matrix(0,ncol=1,nrow=nrow(y))
z<-matrix(0,ncol=ncol(y),nrow=nrow(y))

for(j in 1:a[2]){
  for(i in 1:(a[1]/n)){
   y[i,j]<-mean(x[(i*n-(n-1)):(i*n),j])
 }
}

for (i in 1:ncol(y)){
  means[1,i]=mean(y[,i])}

for (i in 1:nrow(y)){ 
   for (j in 1:ncol(y)){ 
    dif[i,j]<-y[i,j]-means[,j]
    }
}
cova<-covariance(x,n)
ci[1]<-sqrt(dif[1,]%*%solve((cova/n))%*%dif[1,])
ifelse (ci[1]>k,(s[1,]=(s[1,]+dif[1,])*(1-k/ci[1])),(s[1,]=matrix(0,ncol=ncol(y)))) #compute S
  
for (i in 2:nrow(y)){ 
z[i,]<- (s[i-1,]+dif[i,])%*%solve((cova/n))
ci[i,]=sqrt(z[i,]%*%(s[i-1,]+dif[i,])) 

if (ci[i]>k){ s[i,]=(s[i-1,]+dif[i,])*(1-k/ci[i])}
else (s[i,]=matrix(0,ncol=ncol(y)))}

t2=0
for (i in 1:nrow(y)){
  t2[i]=sqrt(s[i,]%*%solve((cova/n))%*%(s[i,]))
  
  } 

 Sample<-seq(1,ncol(t(t2)))
 Samplex<-cbind(Sample,t2)
 subs=subset(Samplex,Samplex[,2]>h)
 
 
plot(t2,type="o",lty=1,ylim=c(0,(max(c(max(t2),h+1)))),pch=16,cex=0.65,main="MCUSUM Control Chart",xlab="Sample",ylab="t2 value",col=ifelse(t2>h,2,1))
abline(h=h)
text(5,h+0.5,paste("UCL =",h))
outList = list ("Multivariate Cumulative Sum (MCUSUM) Control Chart","Upper Control Limits(UCL)"=h,t2=t2,covariance=cova,"The following(s) point(s) fall outside of the control limits"= subs)
 return(outList)
}

