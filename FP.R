library(xgboost)
library(ggplot2)
library(RColorBrewer)
library(psych)

fp.rmse.train = rep(0,5)
fp.rmse.test = rep(0,5)
fp.aae.train = rep(0,5)
fp.aae.test = rep(0,5)
fp.loo = rep(0,5)
fp.ext = rep(0,5)
fp.r2 = rep(0,5)
fp.r = rep(0,5)
fp.all = data.frame(x=integer(),pred=double(),exp=double(),err=double())
fp.all1 = data.frame(x=integer(),pred=double(),exp=double(),err=double())

for(j in 1:5){
  labels = all_FP$FP[folds!=j]
  labels = log(labels+30)
  labels1 = all_FP$FP[folds==j]
  labels1 = log(labels1+30)
  fp.fold = data.frame(x=rep(j,length(labels1)),pred=rep(0,length(labels1)),exp=labels1,err=rep(0,length(labels1)))
  fp.fold1 = data.frame(x=rep(j+5,length(labels)),pred=rep(0,length(labels)),exp=labels,err=rep(0,length(labels)))
  train0 = all_FP1[folds!=j, ]
  test0 = all_FP1[folds==j, ]
  dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
  dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.4, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
  fp.fit = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  fp.pred.train = predict(fp.fit,newdata = dtrain0)
  fp.pred.test = predict(fp.fit,newdata = dtest0)
  fp.fold$pred = fp.pred.test
  fp.fold1$pred = fp.pred.train
  fp.err.train = fp.pred.train - labels
  fp.err.test = fp.pred.test - labels1
  fp.fold$err = fp.err.test
  fp.fold1$err = fp.err.train
  fp.all = rbind(fp.all,fp.fold)
  fp.all1 = rbind(fp.all1,fp.fold1)
  fp.pred.test = exp(fp.pred.test)-30
  fp.pred.train = exp(fp.pred.train)-30
  fp.err.train = fp.pred.train - all_FP$FP[folds!=j]
  fp.err.test = fp.pred.test - all_FP$FP[folds==j]
  fp.rmse.train[j] = rmse(fp.err.train)
  fp.rmse.test[j] = rmse(fp.err.test)
  fp.aae.train[j] = aae(fp.err.train)
  fp.aae.test[j] = aae(fp.err.test)
  avg.train = mean(all_FP$FP[folds!=j])
  fp.r[j] = cor(fp.pred.test,all_FP$FP[folds==j])
  fp.r2[j] = fp.r[j]^2
}

cv.rmse.train = sum(fp.rmse.train)/5 
cv.rmse.test = sum(fp.rmse.test)/5
cv.aae.train = sum(fp.aae.train)/5 
cv.aae.test = sum(fp.aae.test)/5
cv.r2 = sum(fp.r2)/5
cv.r = sum(fp.r)/5

cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.r,cv.r2)
names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","R","R^2")

cv

###########Supplementary
#Step 2
all_FP2$exp = fp.all$exp
all_FP2$pred = fp.all$pred
all_FP2$err = fp.all$err

fp = ggplot(data = fp.all2,mapping = aes(x = exp,y = pred,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_abline(alpha = 0.6) +
  labs(x = expression(log('FP')[exp]),y = expression(log('FP')[pred]),title = "FP Model 5-Fold CV Performance",
       subtitle = "Database Size = 1458, Fold Size = 292") +
  scale_y_continuous(limits = c(-20,100),breaks=seq(-40,100,10)) +
  scale_x_continuous(limits = c(-20,100),breaks=seq(-40,100,10)) +
  scale_color_manual(values=c(brewer.pal(5,"Paired")[1:5]),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp


fp.res = ggplot(data = fp.all2,mapping = aes(x = exp,y = err,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-1),alpha=0.3) +
  geom_hline(aes(yintercept=1),alpha=0.3) +
  labs(x = expression(log('FP')[exp]),y = "Residual",title = "Residual Plot",
       subtitle = "Database Size = 1458, Fold Size = 292") +
  scale_x_continuous(limits = c(-20,100),breaks=seq(-40,100,10)) +
  scale_y_continuous(limits = c(-2,2),breaks=seq(-10,10,0.5)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp.res

cv.rmse.train = sum(fp.rmse.train)/5 
cv.rmse.test = sum(fp.rmse.test)/5
cv.aae.train = sum(fp.aae.train)/5 
cv.aae.test = sum(fp.aae.test)/5
cv.loo = sum(fp.loo)/5 
cv.ext = sum(fp.ext)/5
cv.r2 = sum(fp.r2)/5
cv.r = sum(fp.r)/5

cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.loo,cv.ext,cv.r,cv.r2)
names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","Q^2_loo","Q^2_ext","R","R^2")

cv

######### Willanms Plot

avg = mean(fp.all$pred)
std_err = sd(fp.all$err)*2
fp.all$std = fp.all$err/std_err
fp.all$leverage0 = (fp.all$pred - avg)^2
fp.all$leverage = 1/nrow(fp.all)+fp.all$leverage0/sum(fp.all$leverage0)

fp.wil = ggplot(data = fp.all,mapping = aes(x = leverage,y = std,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*6/1168),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "FP Williams Plot") +
  scale_x_continuous(limits = c(0,0.016),breaks=seq(0,45,0.002)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp.wil

#############################
labels = train$FP
labels = log(labels+30)
labels1 = test$FP
labels1 = log(labels1+30)
train0 = train[,-c(1:48)]
test0 = test[,-c(1:48)]

dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)  

######### eta Tune
eta_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=3, min_child_weight=1, subsample=1, colsample_bytree=1)
  fp.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=fp.fit$evaluation_log
  log$eta = e
  eta_log0 = rbind(eta_log0,log,use.names=FALSE)
}

fp.eta0 = ggplot(data = eta_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "FP Learning Rate Tuning", subtitle="max_depth=3",color='eta') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,10,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp.eta0

######### Max_Depth Tune
max_depth_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  fp.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=fp.fit$evaluation_log
  log$max_depth = d
  max_depth_log0 = rbind(max_depth_log0,log,use.names=FALSE)
}

fp.depth0 = ggplot(data = max_depth_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "FP Interaction Depth Tuning", subtitle="eta=0.1",color='max_depth') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,10,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp.depth0

######### Best0
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=3, min_child_weight=1, subsample=1, colsample_bytree=1)
fp.fit0 = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 10, early_stop_round = 10, maximize = F)

######### Feature Importance Plot
names = colnames(all_FP[49:1137])
importance_matrix = xgb.importance(names, model = fp.fit0)
importance_matrix
#write.csv(importance_matrix,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/Importance Matrix_FP.csv", row.names = FALSE)
xgb.ggplot.importance(importance_matrix[1:10,])+
  labs(title = "FP Descriptor Importance") 
feature = importance_matrix$Feature[1:5]

######### Data Post Process
train1 = train0[,feature]
test1 = test0[,feature]

######### Correalation Plot
cor = cbind(all_FP$FP,log(all_FP$FP+30),all_FP[,feature])
colnames(cor) = c("FP","log(FP)","Mor10v","NsCH3","MATS2dv","Mor10m","SIC5")

pairs.panels(cor, 
             method = "pearson",
             hist.col = brewer.pal(9,"Blues")[8],
             density = TRUE,
             ellipses = TRUE,
             cor = FALSE
)

#tab_train = train1
#tab_test = test1
#tab_train$'log(FP_exp)' = labels
#tab_test$'log(FP_exp)' = labels1
#tab_train = cbind(train[,1:14],tab_train)
#tab_test = cbind(test[,1:14],tab_test)
#write.csv(tab_train,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_train_FP.csv", row.names = FALSE)
#write.csv(tab_test,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_test_FP.csv", row.names = FALSE)

dtrain1 = xgb.DMatrix(data = data.matrix(train1),label = labels) 
dtest1 = xgb.DMatrix(data = data.matrix(test1),label = labels1)

######### eta Tune
eta_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)
  fp.fit = xgb.train(params = params, data = dtrain1, nrounds = 1000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=fp.fit$evaluation_log
  log$eta = e
  eta_log1 = rbind(eta_log1,log,use.names=FALSE)
}

fp.eta1 = ggplot(data = eta_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "FP Learning Rate Tuning", subtitle="max_depth=8",color='eta') +
  scale_y_continuous(limits = c(0.1,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp.eta1

######### Max_Depth Tune
max_depth_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.3, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  fp.fit = xgb.train(params = params, data = dtrain1, nrounds = 2000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=fp.fit$evaluation_log
  log$max_depth = d
  max_depth_log1 = rbind(max_depth_log1,log,use.names=FALSE)
}

fp.depth1 = ggplot(data = max_depth_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "FP Interaction Depth Tuning", subtitle="eta=0.3",color='max_depth') +
  scale_y_continuous(limits = c(0.1,0.45),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

fp.depth1

######### Best1
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.4, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
fp.fit1 = xgb.train(params = params, data = dtrain1, nrounds = 100, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 5, early_stop_round = 10, maximize = F)

fp.pred.train = predict(fp.fit1,newdata = dtrain1)
#tab_train$FP_pred = fp.pred.train 
fp.err.train = fp.pred.train - labels
#tab_train$FP_err = fp.err.train
x = rep(0,length(fp.pred.train))
fp.pred.train = data.frame(x,fp.pred.train,fp.err.train,labels)
colnames(fp.pred.train) = c("x", "fp.pred","fp.err","FP")

fp.pred.test = predict(fp.fit1,newdata = dtest1)
#tab_test$FP_pred = fp.pred.test
fp.err.test = fp.pred.test - labels1
#tab_test$FP_err = fp.err.test
x = rep(1,length(fp.pred.test))
fp.pred.test = data.frame(x,fp.pred.test,fp.err.test,labels1)
colnames(fp.pred.test) = c("x", "fp.pred","fp.err","FP")

fp.pred = rbind(fp.pred.train,fp.pred.test)

#########Plot
fp = ggplot(data = fp.pred,mapping = aes(x = FP,y = fp.pred,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_abline(alpha = 0.8) +
  labs(x = expression(log('FP')[exp]),y = expression(log('FP')[pred]),title = "FP Model Performance") +
  scale_y_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

fp

fp.res = ggplot(data = fp.pred,mapping = aes(x = FP,y = fp.err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-0.2),alpha=0.3) +
  geom_hline(aes(yintercept=+0.2),alpha=0.3) +
  labs(x = expression(log('FP')[exp]),y = "Residual",title = "Residual Plot") +
  scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_y_continuous(limits = c(-0.4,0.4),breaks=seq(-3,3,0.1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

fp.res

######### Willanms Plot

fp.pred.train = predict(fp.fit1,newdata = dtrain1)
fp.pred.test = predict(fp.fit1,newdata = dtest1)
pred.all = c(fp.pred.train,fp.pred.test)
avg = mean(pred.all)
std_err = sqrt(sd(fp.err.test))
x = rep(0,length(fp.pred.train))
err.train = data.frame(x,fp.pred.train,fp.err.train/std_err)
x = rep(1,length(fp.pred.test))
err.test = data.frame(x,fp.pred.test,fp.err.test/std_err)
err.train$leverage0 = (err.train$fp.pred.train - avg)^2
err.train$leverage = 1/length(pred.all)+err.train$leverage0/sum(err.train$leverage0)
err.test$leverage0 = (err.test$fp.pred.test - avg)^2
err.test$leverage = 1/length(pred.all)+err.test$leverage0/sum(err.test$leverage0)
names(err.test) =names(err.train)
error = rbind(err.train,err.test) 


fp.wil = ggplot(data = error,mapping = aes(x = leverage,y = fp.err.train.std_err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*6/103),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "FP Williams Plot") +
  scale_x_continuous(limits = c(0,0.2),breaks=seq(0,45,0.05)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

fp.wil

######### Evaluation
fp.r2.train = cor(fp.pred.train$fp.pred,labels)^2
fp.r2.test = cor(fp.pred.test$fp.pred,labels1)^2
fp.r2.all = cor(fp.pred$fp.pred,fp.pred$`FP`)^2

fp.rmse.train = rmse(fp.pred.train$fp.err)
fp.rmse.test = rmse(fp.pred.test$fp.err)
fp.rmse.all = rmse(fp.pred$fp.err)

fp.aae.train = aae(fp.pred.train$fp.err)
fp.aae.test = aae(fp.pred.test$fp.err)
fp.aae.all = aae(fp.pred$fp.err)

fp.loo = q(fp.pred.train,mean(labels),fp.err.train)
fp.ext = q(fp.pred.test,mean(labels),fp.err.test)

fp.value = c(fp.rmse.train,fp.rmse.test,fp.rmse.all,fp.r2.train,fp.r2.test,fp.r2.all,
              fp.aae.train,fp.aae.test,fp.aae.all,fp.loo,fp.ext)

names(fp.value) = c("RMSE.Train", "RMSE.Test","RMSE.All","R^2.Train","R^2.Test","R^2.All",
                     "AAE.Train","AAE.Test","AAE.All","Q^2_loo","Q^2_ext")

fp.value
