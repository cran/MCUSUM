covariance <-
function(x,n){

a<-dim(x)
a1<-dim(combn(a[2],2))
a2<-t(combn(a[2],2))
y1<-matrix(0,a[1]/n,a[2]) 
y2<-matrix(0,a[1]/n,a1[2])


v1<-matrix(0,a[1]-1,a[2]) 
v2<-matrix(0,a[1]-1,a1[2])


b<-dim(y1)
b1<-dim(v1)

z1<-matrix(0,1,a[2])
z2<-matrix(0,1,a1[2])
z<-matrix(0,a[2],a[2])

w1<-1
w2<-1



if (n>1){
for(j in 1:b[2]){
  for(i in 1:b[1]){
   y1[i,j]<-var(x[(i*n-(n-1)):(i*n),j])
   z1[j]<-mean(y1[,j])
 }
}

for(j in 1:a1[2]){
  for(i in 1:b[1]){
   a3<-a2[j,]
   y2[i,j]<-cov((x[(i*n-(n-1)):(i*n),a3[1]]),(x[(i*n-(n-1)):(i*n),a3[2]]))
   z2[j]<-mean(y2[,j])
  }
}

for(i in 1:b[2]){
  for(j in 1:b[2]){
    ifelse(i==j,z[j,j]<-z1[j],(ifelse((j>i),c(z[i,j]<-z2[w1],w1<-w1+1),q<-0)))
  }
}

for(j in 1:b[2]){
  for(i in 1:b[2]){
    ifelse(i==j,z[j,j]<-z1[j],(ifelse((j<i),c(z[i,j]<-z2[w2],w2<-w2+1),q<-0)))
 
  }
}

}

if (n==1){

for(j in 1:b1[2]){
  for(i in 1:b1[1]){
   v1[i,j]<-x[i+1,j]-x[i,j]
   z1[j]<-(t(v1[,j]))%*%(v1[,j])/(2*(a[1]-1))
 }
}

for(j in 1:a1[2]){
  for(i in 1:b1[1]){
   a3<-a2[j,]
   
   z2[j]<-v1[,a3[1]]%*%v1[,a3[2]]/(2*(a[1]-1))
  }
}

for(i in 1:b1[2]){
  for(j in 1:b1[2]){
    ifelse(i==j,z[j,j]<-z1[j],(ifelse((j>i),c(z[i,j]<-z2[w1],w1<-w1+1),q<-0)))
  }
}

for(j in 1:b1[2]){
  for(i in 1:b1[2]){
    ifelse(i==j,z[j,j]<-z1[j],(ifelse((j<i),c(z[i,j]<-z2[w2],w2<-w2+1),q<-0)))
 
  }
}

}


z
}

