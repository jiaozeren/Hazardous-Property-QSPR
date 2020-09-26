library(xgboost)
library(ggplot2)
library(RColorBrewer)
library(psych)

ait.rmse.train = rep(0,5)
ait.rmse.test = rep(0,5)
ait.aae.train = rep(0,5)
ait.aae.test = rep(0,5)
ait.loo = rep(0,5)
ait.ext = rep(0,5)
ait.r2 = rep(0,5)
ait.r = rep(0,5)
ait.all = data.frame(x=integer(),pred=double(),exp=double(),err=double())
ait.all1 = data.frame(x=integer(),pred=double(),exp=double(),err=double())

for(j in 1:5){
  labels = all_AIT$AIT[folds!=j]
  labels = log(labels)
  labels1 = all_AIT$AIT[folds==j]
  labels1 = log(labels1)
  ait.fold = data.frame(x=rep(j,length(labels1)),pred=rep(0,length(labels1)),exp=labels1,err=rep(0,length(labels1)))
  ait.fold1 = data.frame(x=rep(j+5,length(labels)),pred=rep(0,length(labels)),exp=labels,err=rep(0,length(labels)))
  train0 = all_AIT1[folds!=j, ]
  test0 = all_AIT1[folds==j, ]
  dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
  dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)
  ait.fit = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  ait.pred.train = predict(ait.fit,newdata = dtrain0)
  ait.pred.test = predict(ait.fit,newdata = dtest0)
  ait.fold$pred = ait.pred.test
  ait.fold1$pred = ait.pred.train
  ait.err.train = ait.pred.train - labels
  ait.err.test = ait.pred.test - labels1
  ait.fold$err = ait.err.test
  ait.fold1$err = ait.err.train
  ait.all = rbind(ait.all,ait.fold)
  ait.all1 = rbind(ait.all1,ait.fold1)
  ait.pred.test = exp(ait.pred.test)
  ait.pred.train = exp(ait.pred.train)
  ait.err.train = ait.pred.train - exp(labels)
  ait.err.test = ait.pred.test - all_AIT$AIT[folds==j]
  ait.rmse.train[j] = rmse(ait.err.train)
  ait.rmse.test[j] = rmse(ait.err.test)
  ait.aae.train[j] = aae(ait.err.train)
  ait.aae.test[j] = aae(ait.err.test)
  avg.train = mean(all_AIT$AIT[folds!=j])
  ait.r[j] = cor(ait.pred.test,all_AIT$AIT[folds==j])
  ait.r2[j] = ait.r[j]^2
}

cv.rmse.train = sum(ait.rmse.train)/5 
cv.rmse.test = sum(ait.rmse.test)/5
cv.aae.train = sum(ait.aae.train)/5 
cv.aae.test = sum(ait.aae.test)/5
cv.r2 = sum(ait.r2)/5
cv.r = sum(ait.r)/5

cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.r,cv.r2)
names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","R","R^2")

cv


###########Supplementary
#Step 2
all_AIT2$exp = ait.all$exp
all_AIT2$pred = ait.all$pred
all_AIT2$err = ait.all$err

ait = ggplot(data = ait.all,mapping = aes(x = exp,y = pred,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_abline(alpha = 0.6) +
  labs(x = expression(log('AIT')[exp]),y = expression(log('AIT')[pred]),title = "AIT Model 5-Fold CV Performance",
       subtitle = "Database Size = 188, Fold Size = 38") +
  scale_y_continuous(limits = c(5.25,6.5),breaks=seq(0,45,0.25)) +
  scale_x_continuous(limits = c(5.25,6.5),breaks=seq(0,45,0.25)) +
  scale_color_manual(values=c(brewer.pal(5,"Paired")[1:5]),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait


ait.res = ggplot(data = ait.all,mapping = aes(x = exp,y = err,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-0.4),alpha=0.3) +
  geom_hline(aes(yintercept=+0.4),alpha=0.3) +
  labs(x = expression(log('AIT')[exp]),y = "Residual",title = "Residual Plot",
       subtitle = "Database Size = 188, Fold Size = 38") +
  scale_x_continuous(limits = c(5.25,6.5),breaks=seq(0,45,0.25)) +
  scale_y_continuous(limits = c(-1,1),breaks=seq(-3,3,0.2)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait.res

cv.rmse.train = sum(ait.rmse.train)/5 
cv.rmse.test = sum(ait.rmse.test)/5
cv.aae.train = sum(ait.aae.train)/5 
cv.aae.test = sum(ait.aae.test)/5
cv.loo = sum(ait.loo)/5 
cv.ext = sum(ait.ext)/5
cv.r2 = sum(ait.r2)/5
cv.r = sum(ait.r)/5

cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.loo,cv.ext,cv.r,cv.r2)
names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","Q^2_loo","Q^2_ext","R","R^2")

cv

######### Willanms Plot

avg = mean(ait.all$pred)
std_err = sd(ait.all$err)*2
ait.all$std = ait.all$err/std_err
ait.all$leverage0 = (ait.all$pred - avg)^2
ait.all$leverage = 1/nrow(ait.all)+ait.all$leverage0/sum(ait.all$leverage0)

ait.wil = ggplot(data = ait.all,mapping = aes(x = leverage,y = std,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*5/152),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "AIT Williams Plot") +
  scale_x_continuous(limits = c(0,0.1),breaks=seq(0,45,0.01)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait.wil

#############################
labels = train$AIT
labels = log(labels)
labels1 = test$AIT
labels1 = log(labels1)
train0 = train[,-c(1:21)]
test0 = test[,-c(1:21)]

dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)  

######### eta Tune
eta_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
  ait.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ait.fit$evaluation_log
  log$eta = e
  eta_log0 = rbind(eta_log0,log,use.names=FALSE)
}

ait.eta0 = ggplot(data = eta_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "AIT Learning Rate Tuning", subtitle="max_depth=10",color='eta') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,10,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait.eta0

######### Max_Depth Tune
max_depth_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  ait.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ait.fit$evaluation_log
  log$max_depth = d
  max_depth_log0 = rbind(max_depth_log0,log,use.names=FALSE)
}

ait.depth0 = ggplot(data = max_depth_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "AIT Interaction Depth Tuning", subtitle="eta=0.1",color='max_depth') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,10,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait.depth0

######### Best0
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
ait.fit0 = xgb.train(params = params, data = dtrain0, nrounds = 400, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 10, early_stop_round = 10, maximize = F)

######### Feature Importance Plot
names = colnames(all_AIT[22:1056])
importance_matrix = xgb.importance(names, model = ait.fit0)
importance_matrix
#write.csv(importance_matrix,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/Importance Matrix_AIT.csv", row.names = FALSE)
xgb.ggplot.importance(importance_matrix[1:10,])+
  labs(title = "AIT Descriptor Importance") 
feature = importance_matrix$Feature[1:4]

######### Data Post Process
train1 = train0[,feature]
test1 = test0[,feature]

######### Correalation Plot
cor = cbind(all_AIT$AIT,log(all_AIT$AIT),all_AIT[,feature])
colnames(cor) = c("AIT","log(AIT)","NssCH2","Mor17m","ATSC2Z","Mor10v")

pairs.panels(cor, 
             method = "pearson",
             hist.col = brewer.pal(9,"Blues")[8],
             density = TRUE,
             ellipses = TRUE,
             cor = FALSE
)

#tab_train = train1
#tab_test = test1
#tab_train$'log(AIT_exp)' = labels
#tab_test$'log(AIT_exp)' = labels1
#tab_train = cbind(train[,1:14],tab_train)
#tab_test = cbind(test[,1:14],tab_test)
#write.csv(tab_train,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_train_AIT.csv", row.names = FALSE)
#write.csv(tab_test,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_test_AIT.csv", row.names = FALSE)

dtrain1 = xgb.DMatrix(data = data.matrix(train1),label = labels) 
dtest1 = xgb.DMatrix(data = data.matrix(test1),label = labels1)

######### eta Tune
eta_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)
  ait.fit = xgb.train(params = params, data = dtrain1, nrounds = 1000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ait.fit$evaluation_log
  log$eta = e
  eta_log1 = rbind(eta_log1,log,use.names=FALSE)
}

ait.eta1 = ggplot(data = eta_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "AIT Learning Rate Tuning", subtitle="max_depth=8",color='eta') +
  scale_y_continuous(limits = c(0.05,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait.eta1

######### Max_Depth Tune
max_depth_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  ait.fit = xgb.train(params = params, data = dtrain1, nrounds = 2000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=ait.fit$evaluation_log
  log$max_depth = d
  max_depth_log1 = rbind(max_depth_log1,log,use.names=FALSE)
}

ait.depth1 = ggplot(data = max_depth_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "AIT Interaction Depth Tuning", subtitle="eta=0.1",color='max_depth') +
  scale_y_continuous(limits = c(0.05,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

ait.depth1

######### Best1
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)
ait.fit1 = xgb.train(params = params, data = dtrain1, nrounds = 200, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 5, early_stop_round = 10, maximize = F)

ait.pred.train = predict(ait.fit1,newdata = dtrain1)
#tab_train$AIT_pred = ait.pred.train 
ait.err.train = ait.pred.train - log(train$AIT)
#tab_train$AIT_err = ait.err.train
x = rep(0,length(ait.pred.train))
ait.pred.train = data.frame(x,ait.pred.train,ait.err.train,train$AIT)
colnames(ait.pred.train) = c("x", "ait.pred","ait.err","AIT")

ait.pred.test = predict(ait.fit1,newdata = dtest1)
#tab_test$AIT_pred = ait.pred.test
ait.err.test = ait.pred.test - log(test$AIT)
#tab_test$AIT_err = ait.err.test
x = rep(1,length(ait.pred.test))
ait.pred.test = data.frame(x,ait.pred.test,ait.err.test,test$AIT)
colnames(ait.pred.test) = c("x", "ait.pred","ait.err","AIT")

ait.pred = rbind(ait.pred.train,ait.pred.test)

#########Plot
ait = ggplot(data = ait.pred,mapping = aes(x = log(AIT),y = ait.pred,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_abline(alpha = 0.8) +
  labs(x = expression(log('AIT')[exp]),y = expression(log('AIT')[pred]),title = "AIT Model Performance") +
  scale_y_continuous(limits = c(5.25,6.5),breaks=seq(0,45,0.25)) +
  scale_x_continuous(limits = c(5.25,6.5),breaks=seq(0,45,0.25)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

ait

ait.res = ggplot(data = ait.pred,mapping = aes(x = log(AIT),y = ait.err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-0.2),alpha=0.3) +
  geom_hline(aes(yintercept=+0.2),alpha=0.3) +
  labs(x = expression(log('AIT')[exp]),y = "Residual",title = "Residual Plot") +
  scale_x_continuous(limits = c(2,4.5),breaks=seq(0,45,0.5)) +
  scale_y_continuous(limits = c(-0.4,0.4),breaks=seq(-3,3,0.1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

ait.res

######### Willanms Plot

ait.pred.train = predict(ait.fit1,newdata = dtrain1)
ait.pred.test = predict(ait.fit1,newdata = dtest1)
pred.all = c(ait.pred.train,ait.pred.test)
avg = mean(pred.all)
std_err = sqrt(sd(ait.err.test))
x = rep(0,length(ait.pred.train))
err.train = data.frame(x,ait.pred.train,ait.err.train/std_err)
x = rep(1,length(ait.pred.test))
err.test = data.frame(x,ait.pred.test,ait.err.test/std_err)
err.train$leverage0 = (err.train$ait.pred.train - avg)^2
err.train$leverage = 1/length(pred.all)+err.train$leverage0/sum(err.train$leverage0)
err.test$leverage0 = (err.test$ait.pred.test - avg)^2
err.test$leverage = 1/length(pred.all)+err.test$leverage0/sum(err.test$leverage0)
names(err.test) =names(err.train)
error = rbind(err.train,err.test) 


ait.wil = ggplot(data = error,mapping = aes(x = leverage,y = ait.err.train.std_err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*6/103),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "AIT Williams Plot") +
  scale_x_continuous(limits = c(0,0.2),breaks=seq(0,45,0.05)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=103','Test Set n=35')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=103','Test Set n=35')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

ait.wil

######### Evaluation
ait.r2.train = cor(ait.pred.train$ait.pred,log(ait.pred.train$`AIT`))^2
ait.r2.test = cor(ait.pred.test$ait.pred,log(ait.pred.test$`AIT`))^2
ait.r2.all = cor(ait.pred$ait.pred,log(ait.pred$`AIT`))^2

ait.rmse.train = rmse(ait.pred.train$ait.err)
ait.rmse.test = rmse(ait.pred.test$ait.err)
ait.rmse.all = rmse(ait.pred$ait.err)

ait.aae.train = aae(ait.pred.train$ait.err)
ait.aae.test = aae(ait.pred.test$ait.err)
ait.aae.all = aae(ait.pred$ait.err)

ait.loo = q(ait.pred.train,mean(labels),ait.err.train)
ait.ext = q(ait.pred.test,mean(labels),ait.err.test)

ait.value = c(ait.rmse.train,ait.rmse.test,ait.rmse.all,ait.r2.train,ait.r2.test,ait.r2.all,
              ait.aae.train,ait.aae.test,ait.aae.all,ait.loo,ait.ext)

names(ait.value) = c("RMSE.Train", "RMSE.Test","RMSE.All","R^2.Train","R^2.Test","R^2.All",
                     "AAE.Train","AAE.Test","AAE.All","Q^2_loo","Q^2_ext")

ait.value
