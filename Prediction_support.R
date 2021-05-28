source("Automatically_make_groups.R")
source("Overlap_LARS.R")

#-------------------------------------------------------------------------------
#Calculates RMSE
#-------------------------------------------------------------------------------
rmse<-function(predicted, true){
  return((sum((predicted-true)^2))/length(true))
}

#-------------------------------------------------------------------------------
#My own implementation of SOGL, based on the SGL function from SGL package
#-------------------------------------------------------------------------------
SGL_overlap<-function(X, y, group, alpha, lambdas, variables){
  cat('Entered SGL_overlap \n')
  var<-as.vector(c(c(unlist(list_group)), c(unlist(list_group_extra))))
  ord <- order(group)
  groupord <- group[ord]
  varord <- var[ord]
  groupb <- cumsum(!duplicated(groupord))
  Xb <- X[, varord]
  coefficients<-data.frame(matrix(ncol=length(lambdas), nrow=(variables+1)))
  data=list(x=Xb,y=y)
  cat('Finished my part of code \n')
  print(Sys.time())
  temp<-SGL(data, index=group, type='linear', alpha=alpha, lambdas = lambdas, standardize=FALSE)
  cat('Finished SGL function \n')
  print(Sys.time())
  for (i in 1:length(lambdas)){
    coefficients[2:(variables+1),i]<-tapply(temp$beta[,i], varord, sum)
  }
  coefficients[1,]<-temp$intercept
  return(list(result=temp, selected=apply(temp$beta, 2, function(i) unique(varord[!i==0])), coefficients=coefficients))
}

#-------------------------------------------------------------------------------
#Runs Lasso, returns support and coefficients
#-------------------------------------------------------------------------------
fit_lasso<-function(df, variables){
  #Elastic Net seems to perform even better!!
  answer_lasso<-glmnet(x=as.matrix(df[!names(df) %in% c("y")]),
                       y=as.matrix(df['y']),
                       alpha=1, 
                       lambda=lambdas)
  #Heatmap for Lasso
  heatmap_matrix_lasso<-matrix(0, nrow=variables, ncol=length(lambdas))
  for (i in 1:dim(answer_lasso$beta)[2]){
    temp<-which(answer_lasso$beta[,i]!=0)
    heatmap_matrix_lasso[temp,i]<-1
  }
  return(list(fit=answer_lasso, heatmap_matrix_lasso=heatmap_matrix_lasso))
}

#-------------------------------------------------------------------------------
#Runs SOGL, returns support and coefficients
#-------------------------------------------------------------------------------
fit_SGL<-function(df, variables, alpha=0.5){
  group<-c(rep(1:(length(list_group)+length(list_group_extra)), c(lengths(list_group), lengths(list_group_extra))))
  answer_SGL<-SGL_overlap(X=as.matrix(df[!names(df) %in% c("y")]),
                          y=as.matrix(df['y']),
                          group=group,
                          alpha=alpha, 
                          lambda=lambdas, variables=variables)
  #Heatmap for Overlap Group Lasso
  heatmap_matrix_SGL<-matrix(0, nrow=variables, ncol=length(lambdas))
  for (i in 1:length(lambdas)){
    heatmap_matrix_SGL[answer_SGL$selected[[i]],i]<-1
  }
  return(list(fit=answer_SGL, heatmap_matrix_SGL=heatmap_matrix_SGL))
}



#-------------------------------------------------------------------------------
#Runs OGL, returns support and coefficients
#-------------------------------------------------------------------------------
fit_overlap<-function(df, list_group, list_group_extra, variables){
  group<-c(rep(1:(length(list_group)+length(list_group_extra)), c(lengths(list_group), lengths(list_group_extra))))
  var<-as.vector(c(unlist(list_group), unlist(list_group_extra)))
  weights<-as.vector(c(lengths(list_group), lengths(list_group_extra))^weights_power)
  answer_overlap<-overlapgglasso2(X=as.matrix(df[!names(df) %in% c("y")]), 
                                  y=as.matrix(df['y']), 
                                  var=var, weigh=weights,
                                  group=group, lambda=lambdas)
  #Heatmap for Overlap Group Lasso
  heatmap_matrix_overlap<-matrix(0, nrow=variables, ncol=length(lambdas))
  for (i in 1:length(lambdas)){
    heatmap_matrix_overlap[answer_overlap$var[[i]],i]<-1
  }
  return(list(fit=answer_overlap, heatmap_matrix_overlap=heatmap_matrix_overlap))
}

#-------------------------------------------------------------------------------
#Plots support for OGL
#-------------------------------------------------------------------------------
plot_heatmap_overlap<-function(df_heatmap){
  cat('In overlap plotting \n')
  overlap_plot<-ggplot(df_heatmap,  aes(x=Lambda, y =Variable))+geom_tile(aes(fill = overlap_final))+
    labs(fill="OGL")+
    annotate("rect", xmin = -10, xmax = 3, 
             ymin = support-0.5, ymax = support+0.5,alpha = 0.1, 
             fill = c("white"))+scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
  return(overlap_plot)
}

#-------------------------------------------------------------------------------
#Plots support for Lasso
#-------------------------------------------------------------------------------
plot_heatmap_lasso<-function(df_heatmap){
  cat('In Lasso plotting \n')
  lasso_plot<-ggplot(df_heatmap,  aes(x=Lambda, y =Variable))+geom_tile(aes(fill = lasso_final))+
    labs(fill="Lasso")+
    annotate("rect", xmin = -10, xmax = 3, 
             ymin = support-0.5, ymax = support+0.5,alpha = 0.1, 
             fill = c("white"))+scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
  return(lasso_plot)
}

#-------------------------------------------------------------------------------
#Plots support for SOGL
#-------------------------------------------------------------------------------
plot_heatmap_SGL<-function(df_heatmap){
  ggplot(df_heatmap,  aes(x=Lambda, y =Variable))+geom_tile(aes(fill = SGL_final))+
    labs(fill="SOGL")+
    annotate("rect", xmin = -10, xmax = 3, 
             ymin = support-0.5, ymax = support+0.5,alpha = 0.1, 
             fill = c("white"))+scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
}

#-------------------------------------------------------------------------------
#Plots support for overlap_LARS
#-------------------------------------------------------------------------------
plot_heatmap_overlap_LARS<-function(df_heatmap, list_group){
  cat('In LARS plotting \n')
  ggplot(df_heatmap,  aes(x=-Step, y =Variables))+geom_tile(aes(fill = overlap_LARS_final))+
    labs(fill="Overlap LARS", x='Step')+
    annotate("rect", xmin = -length(list_group), xmax = -1, 
             ymin = support-0.5, ymax = support+0.5,alpha = 0.1, 
             fill = c("white"))+scale_x_discrete(expand=c(0,0))+
    scale_y_discrete(expand=c(0,0))
}

#-------------------------------------------------------------------------------
#Calculates and plots support for the 4 methods based on multiple runs
#-------------------------------------------------------------------------------
plot_heatmaps<-function(list_group, list_group_extra, support, rho, N, error_rate_var, error_rate_y, B=10, SGL=FALSE){
  variables<-length(unique(unlist(list_group)))
  df_heatmap<-expand.grid(1:variables,log(lambdas))
  colnames(df_heatmap)[1:2]<-c('Variable', 'Lambda')
  
  list_group_final<-c(list_group,list_group_extra)
  df_heatmap_LARS<-expand.grid(1:variables,1:length(list_group_final), KEEP.OUT.ATTRS = FALSE)
  names(df_heatmap_LARS)<-c('Variables', 'Step')
  df_heatmap_LARS<-df_heatmap_LARS[c('Step', 'Variables')]
  
  temp<-numeric(variables)
  temp[support]<-1
  df_heatmap$support<-temp
  
  for (b in 1:B){
    cat('now at iteration', b, '\n')
    print(Sys.time())
    dataset<-make_group_df(list_group, support, rho, N, error_rate_var, error_rate_y)
    #round(cor(dataset),1)
    overlap_result<-fit_overlap(dataset, list_group, list_group_extra, variables)
    lasso_result<-fit_lasso(dataset, variables) 
    if (SGL==TRUE){
      SGL_result<-fit_SGL(dataset, variables) 
      df_heatmap[paste0('SGL', b)]<-as.vector(SGL_result$heatmap_matrix_SGL)
    }
    df_heatmap[paste0('lasso', b)]<-as.vector(lasso_result$heatmap_matrix_lasso)
    df_heatmap[paste0('overlap', b)]<-as.vector(overlap_result$heatmap_matrix_overlap)
    LARS_result<-overlap_LARS(dataset, list_group_final)
    for (i in 1:ncol(LARS_result$coefficients)){
      if (mean(LARS_result$coefficients[,i]==0)==1) LARS_result$coefficients[,i]<-LARS_result$coefficients[,(i-1)]
    }
    df_heatmap_LARS[paste0('overlap_LARS', b)]<-as.vector(LARS_result$coefficients[-1,])!=0
  }
  
  df_heatmap$lasso_final<-rowMeans(df_heatmap[grep("lasso", names(df_heatmap))])
  df_heatmap$overlap_final<-rowMeans(df_heatmap[grep("overlap", names(df_heatmap))])
  df_heatmap_LARS$overlap_LARS_final<-rowMeans(df_heatmap_LARS[grep("overlap_LARS", names(df_heatmap_LARS))])
  if (SGL==TRUE){
    df_heatmap$SGL_final<-rowMeans(df_heatmap[grep("SGL", names(df_heatmap))])
    plot_SGL<-plot_heatmap_SGL(df_heatmap)
  }
  
  plot_overlap<-plot_heatmap_overlap(df_heatmap)
  plot_lasso<-plot_heatmap_lasso(df_heatmap)
  plot_LARS<-plot_heatmap_overlap_LARS(df_heatmap_LARS, list_group)
  if (SGL==TRUE){
    return(list(plot_overlap=plot_overlap, plot_lasso=plot_lasso, plot_LARS=plot_LARS, plot_SGL=plot_SGL))
  }
  return(list(plot_overlap=plot_overlap, plot_lasso=plot_lasso, plot_LARS=plot_LARS))
}

#-------------------------------------------------------------------------------
#Calculates and plots prediction for the 4 methods based on Cross-validation
#-------------------------------------------------------------------------------
plot_prediction<-function(CV, list_group,list_group_extra, support, rho, N, error_rate_var, error_rate_y, SOGL=FALSE){
  variables<-length(unique(unlist(list_group)))
  list_group_final<-c(list_group,list_group_extra)
  dataset<-make_group_df(list_group, support, rho, N, error_rate_var, error_rate_y)
  df_rmse<-data.frame(lambda=log(lambdas))
  df_rmse_LARS<-data.frame(1:length(list_group_final))
  names(df_rmse_LARS)<-c('Step')
  indexes<-sample(rep(1:CV, each=N/CV))

  #Loop for CV
  for (cv in 1:CV){
    cat('In CV', cv, '\n')
    train<-dataset[indexes!=cv,]
    test<-dataset[indexes==cv,]
    
    #Lasso prediction
    lasso_result<-fit_lasso(train, variables) 
    lasso_predict<-predict(lasso_result$fit, as.matrix(test[!names(test) %in% c("y")]))
    lasso_rmse<-numeric(length(lambdas))
    for (i in 1:length(lambdas)){
      lasso_rmse[i]<-rmse(lasso_predict[,i],as.matrix(test['y']) )
    }
    df_rmse[paste0('lasso_rmse', cv)]<-lasso_rmse
    
    if (SOGL==TRUE){
      #SOGL prediction
      SOGL_result<-fit_SGL(train, variables) 
      SOGL_result$fit$coefficients
      df_rmse[paste0('SOGL_rmse', cv)]<-apply(SOGL_result$fit$coefficients, 2, function(i) 
        rmse(cbind(1,as.matrix(test[!names(test) %in% c("y")])) %*% i, test$y))
    }

    
    #Overlap Prediction
    overlap_result<-fit_overlap(train, list_group, list_group_extra, variables)
    overlap_predict<-predict(overlap_result$fit, as.matrix(test[!names(test) %in% c("y")]))
    overlap_rmse<-numeric(length(lambdas))
    for (i in 1:length(lambdas)){
      overlap_rmse[i]<-rmse(overlap_predict[,i],as.matrix(test['y']) )
    }
    df_rmse[paste0('overlap_rmse', cv)]<-overlap_rmse
    
    #LARS prediction
    LARS_result<-overlap_LARS(train, list_group_final)
    for (i in 1:ncol(LARS_result$coefficients)){
      if (mean(LARS_result$coefficients[,i]==0)==1) LARS_result$coefficients[,i]<-LARS_result$coefficients[,(i-1)]
    }
    df_rmse_LARS[paste0('overlap_LARS', cv)]<-apply(LARS_result$coefficients, 2, function(i) 
      rmse(cbind(1,as.matrix(test[!names(test) %in% c("y")])) %*% i, test$y))
  }
  
  #Average loops & determine SD's
  df_rmse$lasso_rmse_final<-rowMeans(df_rmse[grep("lasso", names(df_rmse))])
  df_rmse$lasso_sd<-rowSds(as.matrix(df_rmse[grep("lasso", names(df_rmse))]))
  if (SOGL==TRUE){
    df_rmse$SOGL_rmse_final<-rowMeans(df_rmse[grep("SOGL", names(df_rmse))])
    df_rmse$SOGL_sd<-rowSds(as.matrix(df_rmse[grep("SOGL", names(df_rmse))]))
  }
  df_rmse$overlap_rmse_final<-rowMeans(df_rmse[grep("overlap", names(df_rmse))])
  df_rmse$overlap_sd<-rowSds(as.matrix(df_rmse[grep("overlap", names(df_rmse))]))
  df_rmse_LARS$overlap_LARS_rmse<-rowMeans(df_rmse_LARS[grep("overlap_LARS", names(df_rmse_LARS))])
  df_rmse_LARS$overlap_LARS_sd<-rowSds(as.matrix(df_rmse_LARS[grep("overlap_LARS", names(df_rmse_LARS))]))
  
  
  #Plot prediction
  #df_rmse[df_rmse$lambda<1,] for zoom
  plot_all<-ggplot(df_rmse,  aes(x=lambda))+geom_line(aes(y =lasso_rmse_final, color='Lasso'))+
    geom_line(aes(y =overlap_rmse_final, color='OGL'))+labs(x =bquote(exp(lambda)), y = "RMSE")
  #geom_errorbar(aes(ymin=overlap_rmse_final-overlap_sd,ymax=overlap_rmse_final+overlap_sd, color='Overlap'))
  #geom_errorbar(aes(ymin=lasso_rmse_final-lasso_sd,ymax=lasso_rmse_final+lasso_sd, color='Lasso'))

  if (SOGL==TRUE){
    plot_all<-plot_all+geom_line(aes(y =SOGL_rmse_final, color='SOGL'))
    #+geom_errorbar(aes(ymin=SOGL_rmse_final-SOGL_sd,ymax=SOGL_rmse_final+SOGL_sd, color='SOGL'))
  }
  plot_LARS<-ggplot(df_rmse_LARS,  aes(x=Step))+geom_line(aes(y =overlap_LARS_rmse, color='LARS'))+
    scale_x_continuous(breaks=c(1:12))+ labs(y = "RMSE")
  #geom_errorbar(aes(ymin=overlap_LARS_rmse-overlap_LARS_sd,ymax=overlap_LARS_rmse+overlap_LARS_sd, color='LARS'))+ 
  return(list(plot_all=plot_all, plot_LARS=plot_LARS))
  
}