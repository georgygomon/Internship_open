#-------------------------------------------------------------------------------
#My implementation of overlap LARS, based on article: 'RObust groupwise Least Angle
#Regression' by A.Alfons et all and their group LARS package robustHD
#-------------------------------------------------------------------------------
source("Automatically_make_groups.R")
library(RcppArmadillo)
library(robustHD)
library(matrixStats)


quadraticRoots <- function(a, b, c) {
  discriminant <- (b^2) - (4*a*c)
  result<-numeric(2)
  if(discriminant < 0) {
    return(99999)
  }
  else if(discriminant >= 0) {
    result[1] <- (-b + sqrt(discriminant)) / (2*a)
    result[2] <- (-b - sqrt(discriminant)) / (2*a)
    if (length(result[result>= 0])==0){
      return(99999)
    }
    else return(min(result[result>= 0]))
  }
}

#-------------------------------------------------------------------------------
#Group LARS function
#-------------------------------------------------------------------------------
overlap_LARS<-function(unscaled, list_group){
  #Scale both outcome and variables
  X<-as.data.frame(apply(unscaled,2,scale))
  coefficients<-matrix(0, nrow=length(unique(unlist(list_group)))+1, ncol=length(list_group))
  
  #Find group with highest R^2
  z_0_hat<-matrix(0, nrow=length(X$y), ncol=length(list_group))
  R_squared<-numeric(length(list_group))
  counter=0
  for (group in list_group){
    counter=counter+1
    f<-as.formula(paste('y', paste(paste('x', group, sep=''), collapse='+'), sep='~'))
    temp<-lm(f, data=X)
    z_0_hat[,counter]<-temp$fitted.values
    R_squared[counter]=summary(temp)$r.square/length(group)
  }
  #Set active and inactive sets
  active<-which.max(R_squared)
  Non_active<-setdiff(c(1:length(list_group)), active)
  f<-as.formula(paste('y', paste(paste('x', unique(unlist(list_group[active])), sep=''), collapse='+'), sep='~'))
  coefficients[c(1,unique(unlist(list_group[active]))+1),1]<-lm(f, data=X)$coefficients
  
  #Start of iterative algorithm:
  X_A_bar<-matrix(0, nrow=length(X$y), ncol=1)
  
  
  for (k in 1:(length(list_group)-1)){
    if (k==1){
      z_k_hat<-z_0_hat
      z_k_1<-X$y
      X_A_bar<-scale(z_0_hat[,active])
      new=active
    }
    #Standardize fitted values
    else{
      X_A_bar<-cbind(X_A_bar, z_k_hat[,new])
    }
    # cat('At iteration:', k, '\n')
    # cat('Active', active, '\n')
    # cat('Non_active', Non_active, '\n')
    temp<-update_list(active, Non_active, list_group)
    list_group<-temp$list_group
    Non_active<-temp$Non_active
    Deprecated<-temp$Deprecated
    # cat('Active', active, '\n')
    # cat('Non_active', Non_active, '\n')
    # cat('Deprecated', Deprecated, '\n')
    # print(list_group)
    if (sum(lengths(list_group[Non_active]))==0){
      break
    }
    r_k<-cor(z_k_1,X_A_bar[,ncol(X_A_bar)])/sqrt(lengths(list_group[new]))
    R_A<-cor(X_A_bar)
    q_k<-sqrt(lengths(list_group[active]))
    a_k<-as.numeric((t(q_k) %*% solve(R_A) %*% q_k)^(-0.5))
    w_k<-a_k * solve(R_A) %*% q_k
    u_k<-X_A_bar %*% w_k
    
    roots<-numeric(length(Non_active))
    counter=0
    
    u_k_hat<-matrix(0, nrow=length(X$y), ncol=length(list_group))
    for (group in Non_active){
      if(length(list_group[[group]])==0){
        next
      }
      counter=counter+1
      r_kj<-cor(z_k_1, z_k_hat[,group])/sqrt(lengths(list_group[group]))
      a_kj<-cor(u_k, z_k_hat[,group])/sqrt(lengths(list_group[group]))
      f<-as.formula(paste('u_k', paste(paste('x', list_group[[group]], sep=''), collapse='+'), sep='~'))
      data=as.data.frame(cbind(u_k, X))
      u_k_hat[,group]<-lm(f, data=data)$fitted
      tau_k_j<-cor(u_k, u_k_hat[,group])/sqrt(lengths(list_group[group]))
      roots[counter]<-quadraticRoots(a_k^2-tau_k_j^2, 2*(r_kj*a_kj-r_k*a_k), r_k^2-r_kj^2)
    }
    gamma_k<-min(roots)
    # cat('roots', roots, '\n')
    sigma<-sd(z_k_1-gamma_k*u_k)
    z_k_1<-(z_k_1-gamma_k*u_k)/sigma
    for (group in Non_active){
      z_k_hat[,group]<-(z_k_hat[,group]-gamma_k*u_k_hat[,group])/sigma
    }
    
    
    new<-Non_active[which.min(roots)]
    
    active<-union(active, Non_active[which.min(roots)])
    Non_active<-setdiff(c(1:length(list_group)), c(active, Deprecated))
    f<-as.formula(paste('y', paste(paste('x', unique(unlist(list_group[active])), sep=''), collapse='+'), sep='~'))
    coefficients[c(1,unique(unlist(list_group[active]))+1),(k+1)]<-lm(f, data=unscaled)$coefficients
    
    
  }
  return(list(order=c(active), Non_active=Non_active, Deprecated=Deprecated, coefficients=coefficients))
}

#-------------------------------------------------------------------------------
#Updates groups to not include already active variables
#-------------------------------------------------------------------------------
update_list<-function(active, Non_active, list_group){
  temp<-unique(unlist(list_group[active]))
  list_temp<-lapply(list_group[Non_active], function(i) setdiff(i, temp))
  list_temp2<-list_group
  list_temp2[Non_active]<-list_temp
  Deprecated<-which(lengths(list_temp2)==0)
  Non_active<-setdiff(Non_active, Deprecated)
  return(list(list_group=list_temp2, Non_active=Non_active, Deprecated=Deprecated))
}