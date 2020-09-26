library(xgboost)
library(ggplot2)
library(RColorBrewer)
library(psych)

  ufl.rmse.train = rep(0,5)
  ufl.rmse.test = rep(0,5)
  ufl.aae.train = rep(0,5)
  ufl.aae.test = rep(0,5)
  ufl.loo = rep(0,5)
  ufl.ext = rep(0,5)
  ufl.r2 = rep(0,5)
  ufl.r = rep(0,5)
  ufl.all = data.frame(x=integer(),pred=double(),exp=double(),err=double())
  ufl.all1 = data.frame(x=integer(),pred=double(),exp=double(),err=double())
  
  for(j in 1:5){
    labels = all_UFL$UFL[folds!=j]
    labels = log(labels)
    labels1 = all_UFL$UFL[folds==j]
    labels1 = log(labels1)
    ufl.fold = data.frame(x=rep(j,length(labels1)),pred=rep(0,length(labels1)),exp=labels1,err=rep(0,length(labels1)))
    ufl.fold1 = data.frame(x=rep(j+5,length(labels)),pred=rep(0,length(labels)),exp=labels,err=rep(0,length(labels)))
    train0 = all_UFL1[folds!=j, ]
    test0 = all_UFL1[folds==j, ]
    dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
    dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)
    params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.4, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
    ufl.fit = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
    ufl.pred.train = predict(ufl.fit,newdata = dtrain0)
    ufl.pred.test = predict(ufl.fit,newdata = dtest0)
    ufl.fold$pred = ufl.pred.test
    ufl.fold1$pred = ufl.pred.train
    ufl.err.train = ufl.pred.train - labels
    ufl.err.test = ufl.pred.test - labels1
    ufl.fold$err = ufl.err.test
    ufl.fold1$err = ufl.err.train
    ufl.all = rbind(ufl.all,ufl.fold)
    ufl.all1 = rbind(ufl.all1,ufl.fold1)
    ufl.pred.test = exp(ufl.pred.test)
    ufl.pred.train = exp(ufl.pred.train)
    ufl.err.train = ufl.pred.train - exp(labels)
    ufl.err.test = ufl.pred.test - all_UFL$UFL[folds==j]
    ufl.rmse.train[j] = rmse(ufl.err.train)
    ufl.rmse.test[j] = rmse(ufl.err.test)
    ufl.aae.train[j] = aae(ufl.err.train)
    ufl.aae.test[j] = aae(ufl.err.test)
    avg.train = mean(all_UFL$UFL[folds!=j])
    ufl.r[j] = cor(ufl.pred.test,all_UFL$UFL[folds==j])
    ufl.r2[j] = ufl.r[j]^2
  }

  cv.rmse.train = sum(ufl.rmse.train)/5 
  cv.rmse.test = sum(ufl.rmse.test)/5
  cv.aae.train = sum(ufl.aae.train)/5 
  cv.aae.test = sum(ufl.aae.test)/5
  cv.r2 = sum(ufl.r2)/5
  cv.r = sum(ufl.r)/5
  
  cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.r,cv.r2)
  names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","R","R^2")
  
  cv
  
  ###########Supplementary
  #Step 2
  all_UFL2$exp = ufl.all$exp
  all_UFL2$pred = ufl.all$pred
  all_UFL2$err = ufl.all$err
  
  ufl = ggplot(data = ufl.all,mapping = aes(x = exp,y = pred,color = factor(x))) +
    geom_point(size = 2,alpha = 1) +
    geom_abline(alpha = 0.6) +
    labs(x = expression(log('UFL')[exp]),y = expression(log('UFL')[pred]),title = "UFL Model 5-Fold CV Performance",
         subtitle = "Database Size = 138, Fold Size = 28") +
    scale_y_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
    scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
    scale_color_manual(values=c(brewer.pal(5,"Paired")[1:5]),labels = c('1','2','3','4','5'),name ="Fold") +
    theme(
      plot.title = element_text(face = "bold"),  
    )
   
  ufl
  
  
  ufl.res = ggplot(data = ufl.all,mapping = aes(x = exp,y = err,color = factor(x))) +
    geom_point(size = 2,alpha = 1) +
    geom_hline(aes(yintercept=0),alpha=0.8) +
    geom_hline(aes(yintercept=-0.4),alpha=0.3) +
    geom_hline(aes(yintercept=+0.4),alpha=0.3) +
    labs(x = expression(log('UFL')[exp]),y = "Residual",title = "Residual Plot",
         subtitle = "Database Size = 138, Fold Size = 28") +
    scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
    scale_y_continuous(limits = c(-1,1),breaks=seq(-3,3,0.2)) +
    scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
    theme(
      plot.title = element_text(face = "bold"),  
    )
  
  ufl.res
  
  ######### Willanms Plot
  
  avg = mean(ufl.all$pred)
  std_err = sd(ufl.all$err)*2
  ufl.all$std = ufl.all$err/std_err
  ufl.all$leverage0 = (ufl.all$pred - avg)^2
  ufl.all$leverage = 1/nrow(ufl.all)+ufl.all$leverage0/sum(ufl.all$leverage0)
  
  ufl.wil = ggplot(data = ufl.all,mapping = aes(x = leverage,y = std,color = factor(x))) +
    geom_point(size = 2,alpha = 1) +
    geom_hline(aes(yintercept=0),alpha=0.3) +
    geom_hline(aes(yintercept=-3),alpha=0.8) +
    geom_hline(aes(yintercept=3),alpha=0.8) +
    geom_vline(aes(xintercept=3*7/112),alpha=0.8) +
    labs(x = "Leverage",y = "Standard Residuals",title = "UFL Williams Plot") +
    scale_x_continuous(limits = c(0,0.19),breaks=seq(0,45,0.02)) +
    scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
    scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
    theme(
      plot.title = element_text(face = "bold"),  
    )
  
  ufl.wil
  
#############################
labels = train$UFL
labels = log(labels)
labels1 = test$UFL
labels1 = log(labels1)
train0 = train[,-c(1:14)]
test0 = test[,-c(1:14)]

dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)  

######### eta Tune
eta_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=3, min_child_weight=1, subsample=1, colsample_bytree=1)
  ufl.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ufl.fit$evaluation_log
  log$eta = e
  eta_log0 = rbind(eta_log0,log,use.names=FALSE)
}

ulf.eta0 = ggplot(data = eta_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "UFL Learning Rate Tuning", subtitle="max_depth=3",color='eta') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,10,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ulf.eta0

######### Max_Depth Tune
max_depth_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.2, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  ufl.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ufl.fit$evaluation_log
  log$max_depth = d
  max_depth_log0 = rbind(max_depth_log0,log,use.names=FALSE)
}

ulf.depth0 = ggplot(data = max_depth_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "UFL Interaction Depth Tuning", subtitle="eta=0.2",color='max_depth') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,10,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ulf.depth0

######### Best0
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.2, gamma=0, max_depth=3, min_child_weight=1, subsample=1, colsample_bytree=1)
ufl.fit0 = xgb.train(params = params, data = dtrain0, nrounds = 400, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 10, early_stop_round = 10, maximize = F)

######### Feature Importance Plot
names = colnames(all_UFL[15:808])
importance_matrix = xgb.importance(names, model = ufl.fit0)
importance_matrix
#write.csv(importance_matrix,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/Importance Matrix_UFL.csv", row.names = FALSE)
xgb.ggplot.importance(importance_matrix[1:10,])+
  labs(title = "UFL Descriptor Importance") 
feature = importance_matrix$Feature[1:6]

######### Data Post Process
train1 = train0[,feature]
test1 = test0[,feature]

######### Correalation Plot
cor = cbind(all_UFL$UFL,log(all_UFL$UFL),all_UFL[,feature])
colnames(cor) = c("UFL","log(UFL)","Mor21p","SMR_VSA5","Mor18","AETA_beta_ns","GeomShapeIndex","ETA_dBeta")

pairs.panels(cor, 
             method = "pearson",
             hist.col = brewer.pal(9,"Blues")[8],
             density = TRUE,
             ellipses = TRUE,
             cor = FALSE
)

#tab_train = train1
#tab_test = test1
#tab_train$'log(UFL_exp)' = labels
#tab_test$'log(UFL_exp)' = labels1
#tab_train = cbind(train[,1:14],tab_train)
#tab_test = cbind(test[,1:14],tab_test)
#write.csv(tab_train,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_train_UFL.csv", row.names = FALSE)
#write.csv(tab_test,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_test_UFL.csv", row.names = FALSE)

dtrain1 = xgb.DMatrix(data = data.matrix(train1),label = labels) 
dtest1 = xgb.DMatrix(data = data.matrix(test1),label = labels1)

######### eta Tune
eta_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
  ufl.fit = xgb.train(params = params, data = dtrain1, nrounds = 1000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ufl.fit$evaluation_log
  log$eta = e
  eta_log1 = rbind(eta_log1,log,use.names=FALSE)
}

ufl.eta1 = ggplot(data = eta_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "UFL Learning Rate Tuning", subtitle="max_depth=10",color='eta') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ufl.eta1

######### Max_Depth Tune
max_depth_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.4, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  ufl.fit = xgb.train(params = params, data = dtrain1, nrounds = 2000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ufl.fit$evaluation_log
  log$max_depth = d
  max_depth_log1 = rbind(max_depth_log1,log,use.names=FALSE)
}

ufl.depth1 = ggplot(data = max_depth_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "UFL Interaction Depth Tuning", subtitle="eta=0.4",color='max_depth') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ufl.depth1

######### Best1
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.4, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
ufl.fit1 = xgb.train(params = params, data = dtrain1, nrounds = 36, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 5, early_stop_round = 10, maximize = F)

ufl.pred.train = predict(ufl.fit1,newdata = dtrain1)
#tab_train$UFL_pred = ufl.pred.train 
ufl.err.train = ufl.pred.train - log(train$UFL)
#tab_train$UFL_err = ufl.err.train
x = rep(0,length(ufl.pred.train))
ufl.pred.train = data.frame(x,ufl.pred.train,ufl.err.train,train$UFL)
colnames(ufl.pred.train) = c("x", "ufl.pred","ufl.err","UFL")

ufl.pred.test = predict(ufl.fit1,newdata = dtest1)
#tab_test$UFL_pred = ufl.pred.test
ufl.err.test = ufl.pred.test - log(test$UFL)
#tab_test$UFL_err = ufl.err.test
x = rep(1,length(ufl.pred.test))
ufl.pred.test = data.frame(x,ufl.pred.test,ufl.err.test,test$UFL)
colnames(ufl.pred.test) = c("x", "ufl.pred","ufl.err","UFL")

ufl.pred = rbind(ufl.pred.train,ufl.pred.test)

#########Plot
ufl = ggplot(data = ufl.pred,mapping = aes(x = log(UFL),y = ufl.pred,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_abline(alpha = 0.8) +
  labs(x = expression(log('UFL')[exp]),y = expression(log('UFL')[pred]),title = "UFL Model Performance") +
  scale_y_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

ufl

ufl.res = ggplot(data = ufl.pred,mapping = aes(x = log(UFL),y = ufl.err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-0.2),alpha=0.3) +
  geom_hline(aes(yintercept=+0.2),alpha=0.3) +
  labs(x = expression(log('UFL')[exp]),y = "Residual",title = "Residual Plot") +
  scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_y_continuous(limits = c(-0.4,0.4),breaks=seq(-3,3,0.1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

ufl.res

######### Willanms Plot

ufl.pred.train = predict(ufl.fit1,newdata = dtrain1)
ufl.pred.test = predict(ufl.fit1,newdata = dtest1)
pred.all = c(ufl.pred.train,ufl.pred.test)
avg = mean(pred.all)
std_err = sqrt(sd(ufl.err.test))
x = rep(0,length(ufl.pred.train))
err.train = data.frame(x,ufl.pred.train,ufl.err.train/std_err)
x = rep(1,length(ufl.pred.test))
err.test = data.frame(x,ufl.pred.test,ufl.err.test/std_err)
err.train$leverage0 = (err.train$ufl.pred.train - avg)^2
err.train$leverage = 1/length(pred.all)+err.train$leverage0/sum(err.train$leverage0)
err.test$leverage0 = (err.test$ufl.pred.test - avg)^2
err.test$leverage = 1/length(pred.all)+err.test$leverage0/sum(err.test$leverage0)
names(err.test) =names(err.train)
error = rbind(err.train,err.test) 


ufl.wil = ggplot(data = error,mapping = aes(x = leverage,y = ufl.err.train.std_err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*6/103),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "UFL Williams Plot") +
  scale_x_continuous(limits = c(0,0.2),breaks=seq(0,45,0.05)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

ufl.wil

######### Evaluation
ufl.r2.train = cor(ufl.pred.train$ufl.pred,log(ufl.pred.train$`UFL`))^2
ufl.r2.test = cor(ufl.pred.test$ufl.pred,log(ufl.pred.test$`UFL`))^2
ufl.r2.all = cor(ufl.pred$ufl.pred,log(ufl.pred$`UFL`))^2

ufl.rmse.train = rmse(ufl.pred.train$ufl.err)
ufl.rmse.test = rmse(ufl.pred.test$ufl.err)
ufl.rmse.all = rmse(ufl.pred$ufl.err)

ufl.aae.train = aae(ufl.pred.train$ufl.err)
ufl.aae.test = aae(ufl.pred.test$ufl.err)
ufl.aae.all = aae(ufl.pred$ufl.err)

ufl.loo = q(ufl.pred.train,mean(labels),ufl.err.train)
ufl.ext = q(ufl.pred.test,mean(labels),ufl.err.test)

ufl.value = c(ufl.rmse.train,ufl.rmse.test,ufl.rmse.all,ufl.r2.train,ufl.r2.test,ufl.r2.all,
              ufl.aae.train,ufl.aae.test,ufl.aae.all,ufl.loo,ufl.ext)

names(ufl.value) = c("RMSE.Train", "RMSE.Test","RMSE.All","R^2.Train","R^2.Test","R^2.All",
                     "AAE.Train","AAE.Test","AAE.All","Q^2_loo","Q^2_ext")

ufl.value
