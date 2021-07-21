#Function for to retrieve test statistic T2 from vectors X and Y.
#This function was developed by Abraham Hussein, with help from Andrew Symes.


Cramer.vMises<- function(X,Y){
#Sorting our vectors from smallest -> largest value.
  X<-sort(X)
  Y<-sort(Y)
#Getting n,m,N (vector lengths and total length).
  n=length(X)
  m=length(Y)
  N=n+m
#The prob. of Death at Xi.
    SX=c()
    k=0
    for (i in 1:n) {
      L<-duplicated(X,nmax = n)
      if(L[i]==FALSE){
        k=k+1
        SX=c(SX,k/n)
      }
      else{
        k=k+1
        SX=replace(SX, SX==(i-1)/n,k/n)
        SX=c(SX,k/n)
      }
      
    }
#The prob. of Death at Yi.
    SY=c()
    k=0
    for (i in 1:m) {
      L<-duplicated(Y,nmax = m)
      if(L[i]==FALSE){
        k=k+1
        SY=c(SY,k/m)
      }
      else{
        k=k+1
        SY=replace(SY, SY==(i-1)/m,k/m)
        SY=c(SY,k/m)
      }
      
    }

  
  #Combing our Vectors, assigning indicators, and attaching death prob.
  z1= append(X,Y)
  si= append(c((1:n)/n),rep((1:m)/m))
  xi= append(c(rep(1,n)),c(rep(0,m)))
  yi= append(c(rep(0,n)),c(rep(1,m)))
  
  #Structuring our Data
  Z=data.frame(cbind(z1,si,xi,yi))
  Z
  
  #Dataframe sorted ordinally, which each obs having Si and ind. assigned.
  df <- Z[order(Z$z1),]
  
  SX.i<-(df$xi*df$si)
  SX.f = c()
  
  SY.i<-(df$yi*df$si)
  SY.f = c()
  
  #For loop Creating S(Xi) and S(Yi).
  
  temps <- c()
  temps <- c(temps,0)
  
  for (i in c(1:N)) {
    temp = SX.i[i]
    if(temp != 0){
      temps <- c(temps,temp)
      SX.f <- c(SX.f,temp)
    }
    else{SX.f <- c(SX.f,temps[length(temps)])}
    
  }
  for (i in c(1:N)) {
    temp = SY.i[i]
    if(temp != 0){
      temps <- c(temps,temp)
      SY.f <- c(SY.f,temp)
    }
    else{SY.f <- c(SY.f,temps[length(temps)])}
    
  }
#T1 (Smirnov) Test Statistic
  T1=max(SX.f-SY.f)
#T2 Test Statistic
  T2=((m*n)/(m+n)^2)*sum((SX.f-SY.f)^2)
  cat("T1 is computed to be ",T1,".\n")
  cat("T2 is computed to be ",T2,".\n")
}

#Example 2, Page 464

X=c(7.6,8.4,8.6,8.7,9.3,9.9,10.1,10.6,11.2)
Y=c(5.2,5.7,5.9,6.5,6.8,8.2,9.1,9.8,10.8,11.3,11.5,12.3,12.5,13.4,14.6)

Cramer.vMises(X,Y)

#Exercise 1, Page 465

X=c(0.6,0.8,0.8,1.2,1.4)
Y=c(1.3,1.3,1.8,2.4,2.9)

Cramer.vMises(X,Y)


