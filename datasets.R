#-------------------------------------------------------------------------------
#Loads Prediction_support.R file where the prediction and support plots
#for each of the 4 methods (Lasso, OGL, SOGL and overlap LARS) are made
#-------------------------------------------------------------------------------
source("Prediction_support.R")




#-------------------------------------------------------------------------------
#Dataset 4_large
#-------------------------------------------------------------------------------
#The groups should be supplied in this format
list_group<-list('1'=c(1:4, 49:50, 55:56, 83:84),
                 '2'=c(5:8, 49:52, 57:58, 83:86),
                 '3'=c(9:12, 51:54, 59:60, 85:88),
                 '4'=c(13:16, 13:16, 53:54, 61:62, 87:88),
                 '5'=c(17:20, 55:58, 63:64, 69:70, 83:84, 89:90),
                 '6'=c(21:24, 57:58, 63:66, 83:86, 89:92),
                 '7'=c(25:28, 59:60, 65:68, 73:74, 85:88, 91:94),
                 '8'=c(29:32, 61:62, 67:68, 75:76, 87:88, 93:94),
                 '9'=c(33:36, 69:70, 77:78, 89:90),
                 '10'=c(37:40, 71:72, 77:80, 89:92),
                 '11'=c(41:44, 73:74, 79:82, 91:94),
                 '12'=c(45:48, 93:94, 75:76, 81:82))
#All subsets and singletons should be supplied in list_group_extra
#Thus all groups which are completely contained in another group should be supplied here,
#Otherwise the function automatically generating the groups does not work
list_group_extra<-list()
support<-unique(c(9:12, 51:54, 59:60, 85:88, 25:28, 59:60, 65:68, 73:74, 85:88, 91:94, 41:44, 73:74, 79:82, 91:94))
#Number of observations
N<-100
#Parameters needed to create dataset, for exact definition see Internship wrap-up report
weights_power<-0.3
error_rate_var<-0.6
rho<-0.5
error_rate_y<-1
#The true regression vector
w<-rep(2, length(support))
#Penalty parameter sequence for Lasso, OGL and SOGL
lambdas<-exp(seq(3, -10, length.out=200))

#Plots support selection of the 4 methods
plots<-plot_heatmaps(list_group=list_group,list_group_extra=list_group_extra, 
                     support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                     error_rate_y=error_rate_y, B=5, SGL=TRUE)

#Plots prediction of the 4 methods
plot_pred<-plot_prediction(CV=6, list_group=list_group,list_group_extra=list_group_extra, 
                           support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                           error_rate_y=error_rate_y, SOGL=TRUE)

#-------------------------------------------------------------------------------
#Dataset 4_small
#-------------------------------------------------------------------------------
list_group<-list('1'=c(1:4, 49:50, 55:56, 83:84),
                 '2'=c(5:8, 49:52, 57:58, 83:86),
                 '3'=c(9:12, 51:54, 59:60, 85:88),
                 '4'=c(13:16, 13:16, 53:54, 61:62, 87:88),
                 '5'=c(17:20, 55:58, 63:64, 69:70, 83:84, 89:90),
                 '6'=c(21:24, 57:58, 63:66, 83:86, 89:92),
                 '7'=c(25:28, 59:60, 65:68, 73:74, 85:88, 91:94),
                 '8'=c(29:32, 61:62, 67:68, 75:76, 87:88, 93:94),
                 '9'=c(33:36, 69:70, 77:78, 89:90),
                 '10'=c(37:40, 71:72, 77:80, 89:92),
                 '11'=c(41:44, 73:74, 79:82, 91:94),
                 '12'=c(45:48, 93:94, 75:76, 81:82))
list_group_extra<-list()

support<-unique(c(9:12, 51:54, 59:60, 85:88))
rho<-0.3
N<-120
weights_power<-0.3
error_rate_var<-0.6
error_rate_y<-1
lambdas<-exp(seq(3, -10, length.out=200))
w<-rep(2, length(support))



plots<-plot_heatmaps(list_group=list_group,list_group_extra=list_group_extra, 
                     support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                     error_rate_y=error_rate_y, B=5, SGL=TRUE)

plot_pred<-plot_prediction(CV=6, list_group=list_group,list_group_extra=list_group_extra, 
                           support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                           error_rate_y=error_rate_y, SOGL=TRUE)

#-------------------------------------------------------------------------------
#Dataset 4_large_overlap
#-------------------------------------------------------------------------------
list_group<-list('1'=c(1:4, 49:52, 61:64, 117:120),
                 '2'=c(5:8, 49:56, 117:124, 65:68),
                 '3'=c(9:12, 53:60, 121:128, 69:72),
                 '4'=c(13:16, 57:60, 73:76, 125:128),
                 '5'=c(17:20, 85:92, 117:120, 137:140),
                 '6'=c(21:24, 65:68, 81:88, 93:96, 117:124, 133:140),
                 '7'=c(25:28, 69:72, 77:84, 97:100, 121:128, 129:136),
                 '8'=c(29:32, 73:80, 101:104, 125:132),
                 '9'=c(33:36, 89:92, 113:116, 137:140),
                 '10'=c(37:40, 93:96, 109:116, 133:140),
                 '11'=c(41:44, 97:100, 105:112, 129:136),
                 '12'=c(45:48, 101:108, 129:132))
list_group_extra<-list()

support<-unique(c(25:28, 69:72, 77:84, 97:100, 121:136))
rho<-0.5
N<-180
weights_power<-0.3
error_rate_var<-0.6
error_rate_y<-1
lambdas<-exp(seq(3, -10, length.out=200))
w<-rep(2, length(support))


plots<-plot_heatmaps(list_group=list_group,list_group_extra=list_group_extra, 
                     support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                     error_rate_y=error_rate_y, B=5, SGL=TRUE)

plot_pred<-plot_prediction(CV=6, list_group=list_group,list_group_extra=list_group_extra, 
                           support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                           error_rate_y=error_rate_y, SOGL=TRUE)

#-------------------------------------------------------------------------------
#Dataset 4_subgroups
#-------------------------------------------------------------------------------
list_group<-list('1'=c(1:4, 49:52, 61:64, 117:120),
                 '2'=c(5:8, 49:56, 117:124, 65:68),
                 '3'=c(9:12, 53:60, 121:128, 69:72),
                 '4'=c(13:16, 57:60, 73:76, 125:128),
                 '5'=c(17:20, 85:92, 117:120, 137:140),
                 '6'=c(21:24, 65:68, 81:88, 93:96, 117:124, 133:140),
                 '7'=c(25:28, 69:72, 77:84, 97:100, 121:128, 129:136),
                 '8'=c(29:32, 73:80, 101:104, 125:132),
                 '9'=c(33:36, 89:92, 113:116, 137:140),
                 '10'=c(37:40, 93:96, 109:116, 133:140),
                 '11'=c(41:44, 97:100, 105:112, 129:136),
                 '12'=c(45:48, 101:108, 129:132))

list_group_extra<-list('13'=c(117:120),
                       '14'=c(121:124),
                       '15'=c(125:128),
                       '16'=c(129:132),
                       '17'=c(133:136),
                       '18'=c(137:140))
support<-unique(c(25:28, 69:72, 77:84, 97:100, 121:136))
rho<-0.5
N<-190
weights_power<-0.3
error_rate_var<-0.6
error_rate_y<-1
lambdas<-exp(seq(3, -10, length.out=200))
w<-rep(2, length(support))


plots<-plot_heatmaps(list_group=list_group,list_group_extra=list_group_extra, 
                     support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                     error_rate_y=error_rate_y, B=5, SGL=TRUE)

plot_pred<-plot_prediction(CV=5, list_group=list_group,list_group_extra=list_group_extra, 
                           support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                           error_rate_y=error_rate_y, SOGL=TRUE)



#-------------------------------------------------------------------------------
#Dataset High degree of overlap
#-------------------------------------------------------------------------------
list_group<-list('1'=c(1:4, 21:26),
                 '2'=c(5:8, 21:22, 27:30, 35:36),
                 '3'=c(9:12, 23:24, 29:32, 35:36),
                 '4'=c(13:16, 31:34, 35:36),
                 '5'=c(17:20, 25:28, 33:34, 35:36))
list_group_extra<-list()

support<-unique(c(1:4, 21:26))
rho<-0.5
N<-100
weights_power<-0.3
error_rate_var<-0.6
error_rate_y<-1
lambdas<-exp(seq(3, -10, length.out=200))
w<-rep(2, length(support))


plots<-plot_heatmaps(list_group=list_group,list_group_extra=list_group_extra, 
                     support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                     error_rate_y=error_rate_y, B=5, SGL=FALSE)

plot_pred<-plot_prediction(CV=10, list_group=list_group,list_group_extra=list_group_extra, 
                           support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                           error_rate_y=error_rate_y, SOGL=TRUE)


#-------------------------------------------------------------------------------
#Dataset High degree of overlap
#-------------------------------------------------------------------------------
list_group<-list('1'=c(1:4, 21:22),
                 '2'=c(5:8, 21:26, 31:32),
                 '3'=c(9:12, 23:24, 27:28, 31:32),
                 '4'=c(13:16, 27:30, 31:32),
                 '5'=c(17:20, 25:26, 29:30, 31:32))
length(unique(unlist(list_group)))
list_group_extra<-list()
# list_group_extra<-list('13'=c(117:120),
#                        '14'=c(121:124),
#                        '15'=c(125:128),
#                        '16'=c(129:132),
#                        '17'=c(133:136),
#                        '18'=c(137:140))
support<-unique(c(1:4, 21:22))
rho<-0.5
N<-100
weights_power<-0.3
error_rate_var<-0.6
error_rate_y<-1
lambdas<-exp(seq(3, -10, length.out=200))
w<-rep(2, length(support))


plots<-plot_heatmaps(list_group=list_group,list_group_extra=list_group_extra, 
                     support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                     error_rate_y=error_rate_y, B=5, SGL=FALSE)

plot_pred<-plot_prediction(CV=10, list_group=list_group,list_group_extra=list_group_extra, 
                           support=support, rho=rho, N=N, error_rate_var=error_rate_var, 
                           error_rate_y=error_rate_y, SOGL=TRUE)
