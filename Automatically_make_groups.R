#-------------------------------------------------------------------------------
#Function that automatically creates groups when given a group_list
#-------------------------------------------------------------------------------
library(MLGL)
library(matrixcalc)
library(gglasso)
library(glmnet)
library(ggplot2)
library(collections)
library(SGL)

#-------------------------------------------------------------------------------
#Adjusted OGL function, original OGL function gave errors
#-------------------------------------------------------------------------------
overlapgglasso2<-function (X, y, var, group, lambda = NULL, weight = NULL, loss = c("ls", 
                                                                                    "logit"), intercept = TRUE, ...) 
{
  loss <- match.arg(loss)
  ord <- order(group)
  groupord <- group[ord]
  varord <- var[ord]
  groupb <- cumsum(!duplicated(groupord))
  Xb <- X[, varord]
  if (is.null(weight)) {
    weight <- as.numeric(sqrt(table(groupb)))
  }
  t1 <- proc.time()
  res <- gglasso(Xb, y, groupb, pf = weight, lambda = lambda, 
                 intercept = intercept, loss = loss, ...)
  t2 <- proc.time()
  res2 <- list()
  res2$lambda <- res$lambda
  non0 <- apply(res$beta, 2, FUN = function(x) {
    which(x != 0)
  })
  res2$var <- lapply(non0, FUN = function(x) {
    varord[x]
  })
  res2$nVar <- sapply(res2$var, FUN = function(x) {
    length(unique(x))
  })
  res2$group <- lapply(non0, FUN = function(x) {
    groupb[x]
  })
  res2$nGroup <- sapply(res2$group, FUN = function(x) {
    length(unique(x))
  })
  res2$beta <- lapply(seq_along(res$lambda), FUN = function(x) {
    res$beta[non0[[x]], x]
  })
  res2$b0 <- res$b0
  res2$structure <- list(group = groupb, var = varord, weight = weight)
  res2$dim <- dim(X)
  res2$time <- (t2 - t1)[3]
  class(res2) <- "MLGL"
  return(res2)
}


cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

#-------------------------------------------------------------------------------
#Creates group dictionary in which for every variable the group(s) it belongs to are shown
#-------------------------------------------------------------------------------
create_group_dict<-function(list_group){
  group_dict<- list()
  number_of_groups<-length(list_group)
  x1<-combn(1:number_of_groups, 1, simplify=FALSE)
  for (j in 1:length(x1)){
    group_dict[[paste0(x1[[j]])]]<-as.vector(setdiff(unlist(list_group[x1[[j]]]), 
                                                     unlist(list_group[-x1[[j]]])))
  }
  if (number_of_groups>=2){
    x2<-combn(1:number_of_groups, 2, simplify=FALSE)
    for (j in 1:length(x2)){
      group_dict[[paste0(x2[[j]][1], sep=',',x2[[j]][2])]]<-as.vector(setdiff(intersect(unlist(list_group[x2[[j]][1]]),
                                                                                        unlist(list_group[x2[[j]][2]])), 
                                                                              unlist(list_group[-c(x2[[j]][2], x2[[j]][1])])
      ))  
  }
  }
  if (number_of_groups>=3){
    x3<-combn(1:number_of_groups, 3, simplify=FALSE)
    for (j in 1:length(x3)){
      group_dict[[paste0(x3[[j]][1], sep=',',x3[[j]][2], sep=',',x3[[j]][3])]]<-
        as.vector(
          setdiff(
            Reduce(intersect, list(unlist(list_group[x3[[j]][1]]),unlist(list_group[x3[[j]][2]]),
                                   unlist(list_group[x3[[j]][3]]))), 
            unlist(list_group[-c(x3[[j]][1], x3[[j]][2], x3[[j]][3])])))
    } 
  }

  if (number_of_groups>=4){
    x4<-combn(1:number_of_groups, 4, simplify=FALSE)
    for (j in 1:length(x4)){
      group_dict[[paste0(x4[[j]][1], sep=',',x4[[j]][2], sep=',',x4[[j]][3], sep=',',x4[[j]][4])]]<-as.vector(setdiff(
            Reduce(intersect, list(unlist(list_group[x4[[j]][1]]),unlist(list_group[x4[[j]][2]]),
                                   unlist(list_group[x4[[j]][3]]),unlist(list_group[x4[[j]][4]]))), 
            unlist(list_group[-c(x4[[j]][1], x4[[j]][2], x4[[j]][3], x4[[j]][4])])))
    }
  }

  return(group_dict)
}

#-------------------------------------------------------------------------------
#Creates the variables based on the group dictionary
#-------------------------------------------------------------------------------
create_df_from_dict<-function(group_dict, list_group, rho, N, error_rate_var, error_rate_y){
  variables<-max(unlist(list_group))
  df <- data.frame(x1=numeric(N)) 
  for (i in 2:variables){
    df[paste0('x', i)]<-numeric(N)
  }
  nams<-strsplit(names(group_dict), split=c(','))
  for (i in 1:length(group_dict)){
    if ((lengths(nams)[i]==1) & (length(group_dict[[i]])>1)){
      df[group_dict[[i]]]<-simuBlockGaussian(N, 1, lengths(group_dict)[i], rho)
    }
    if (lengths(nams)[i]>1){
      temp<-numeric(0)
      for (j in nams[[i]]){
        temp<-cbind.fill(temp, df[group_dict[[j]]])
      }
      temp2<-rowMeans(temp, na.rm = TRUE)
      df[group_dict[[i]]]<-temp2+rnorm(length(group_dict[[i]])*N, 0, error_rate_var*sd(temp2))
    }
  }
  return(df)
}

#-------------------------------------------------------------------------------
#Main function of Automatically_make_groups
#-------------------------------------------------------------------------------
make_group_df<-function(list_group, support, rho=0.5, N=1000, error_rate_var=0.3, error_rate_y=0.7){
  group_dict<-create_group_dict(list_group)
  df<-create_df_from_dict(group_dict, list_group, rho, N, error_rate_var, error_rate_y)
  support_data<-as.matrix(df[support])
  w<-rep(2, length(support))
  df$y<-support_data %*% w +rnorm(N, 0, error_rate_y*sd(support_data))
  return(df)
}


