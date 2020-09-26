library(xgboost)
library(ggplot2)
library(RColorBrewer)
library(psych)

lfl.rmse.train = rep(0,5)
lfl.rmse.test = rep(0,5)
lfl.aae.train = rep(0,5)
lfl.aae.test = rep(0,5)
lfl.loo = rep(0,5)
lfl.ext = rep(0,5)
lfl.r2 = rep(0,5)
lfl.r = rep(0,5)
lfl.all = data.frame(x=integer(),pred=double(),exp=double(),err=double())
lfl.all1 = data.frame(x=integer(),pred=double(),exp=double(),err=double())

for(j in 1:5){
  labels = all_LFL$LFL[folds!=j]
  labels = log(labels)
  labels1 = all_LFL$LFL[folds==j]
  labels1 = log(labels1)
  lfl.fold = data.frame(x=rep(j,length(labels1)),pred=rep(0,length(labels1)),exp=labels1,err=rep(0,length(labels1)))
  lfl.fold1 = data.frame(x=rep(j+5,length(labels)),pred=rep(0,length(labels)),exp=labels,err=rep(0,length(labels)))
  train0 = all_LFL1[folds!=j, ]
  test0 = all_LFL1[folds==j, ]
  dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
  dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.2, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
  lfl.fit = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  lfl.pred.train = predict(lfl.fit,newdata = dtrain0)
  lfl.pred.test = predict(lfl.fit,newdata = dtest0)
  lfl.fold$pred = lfl.pred.test
  lfl.fold1$pred = lfl.pred.train
  lfl.err.train = lfl.pred.train - labels
  lfl.err.test = lfl.pred.test - labels1
  lfl.fold$err = lfl.err.test
  lfl.fold1$err = lfl.err.train
  lfl.all = rbind(lfl.all,lfl.fold)
  lfl.all1 = rbind(lfl.all1,lfl.fold1)
  lfl.pred.test = exp(lfl.pred.test)
  lfl.pred.train = exp(lfl.pred.train)
  lfl.err.train = lfl.pred.train - exp(labels)
  lfl.err.test = lfl.pred.test - all_LFL$LFL[folds==j]
  lfl.rmse.train[j] = rmse(lfl.err.train)
  lfl.rmse.test[j] = rmse(lfl.err.test)
  lfl.aae.train[j] = aae(lfl.err.train)
  lfl.aae.test[j] = aae(lfl.err.test)
  avg.train = mean(all_LFL$LFL[folds!=j])
  lfl.r[j] = cor(lfl.pred.test,all_LFL$LFL[folds==j])
  lfl.r2[j] = lfl.r[j]^2
}

cv.rmse.train = sum(lfl.rmse.train)/5 
cv.rmse.test = sum(lfl.rmse.test)/5
cv.aae.train = sum(lfl.aae.train)/5 
cv.aae.test = sum(lfl.aae.test)/5
cv.r2 = sum(lfl.r2)/5
cv.r = sum(lfl.r)/5

cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.r,cv.r2)
names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","R","R^2")

cv

###########Supplementary
#Step 2
all_LFL2$exp = lfl.all$exp
all_LFL2$pred = lfl.all$pred
all_LFL2$err = lfl.all$err

lfl = ggplot(data = lfl.all,mapping = aes(x = exp,y = pred,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_abline(alpha = 0.6) +
  labs(x = expression(log('LFL')[exp]),y = expression(log('LFL')[pred]),title = "LFL Model 5-Fold CV Performance",
       subtitle = "Database Size = 271, Fold Size = 55") +
  scale_y_continuous(limits = c(0.5,3),breaks=seq(0,45,0.5)) +
  scale_x_continuous(limits = c(0.5,3),breaks=seq(0,45,0.5)) +
  scale_color_manual(values=c(brewer.pal(5,"Paired")[1:5]),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl


lfl.res = ggplot(data = lfl.all,mapping = aes(x = exp,y = err,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-0.4),alpha=0.3) +
  geom_hline(aes(yintercept=+0.4),alpha=0.3) +
  labs(x = expression(log('LFL')[exp]),y = "Residual",title = "Residual Plot",
       subtitle = "Database Size = 271, Fold Size = 55") +
  scale_x_continuous(limits = c(0.5,3),breaks=seq(0,45,0.5)) +
  scale_y_continuous(limits = c(-1,1),breaks=seq(-3,3,0.2)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.res

######### Willanms Plot

avg = mean(lfl.all$pred)
std_err = sd(lfl.all$err)*2
lfl.all$std = lfl.all$err/std_err
lfl.all$leverage0 = (lfl.all$pred - avg)^2
lfl.all$leverage = 1/nrow(lfl.all)+lfl.all$leverage0/sum(lfl.all$leverage0)

lfl.wil = ggplot(data = lfl.all,mapping = aes(x = leverage,y = std,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*6/220),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "LFL Williams Plot") +
  scale_x_continuous(limits = c(0,0.09),breaks=seq(0,45,0.01)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.wil

cv.rmse.train = sum(lfl.rmse.train)/5 
cv.rmse.test = sum(lfl.rmse.test)/5
cv.aae.train = sum(lfl.aae.train)/5 
cv.aae.test = sum(lfl.aae.test)/5
cv.loo = sum(lfl.loo)/5 
cv.ext = sum(lfl.ext)/5
cv.r2 = sum(lfl.r2)/5
cv.r = sum(lfl.r)/5

cv = c(cv.rmse.train,cv.rmse.test,cv.aae.train,cv.aae.test,cv.loo,cv.ext,cv.r,cv.r2)
names(cv) = c("RMSE.Train", "RMSE.Test","AAE.Train","AAE.Test","Q^2_loo","Q^2_ext","R","R^2")

cv
###############################

labels = train$LFL
labels = log(labels)
labels1 = test$LFL
labels1 = log(labels1)
train0 = train[,-c(1:17)]
test0 = test[,-c(1:17)]

dtrain0 = xgb.DMatrix(data = data.matrix(train0),label = labels) 
dtest0 = xgb.DMatrix(data = data.matrix(test0),label = labels1)  

######### eta Tune
eta_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
  lfl.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=lfl.fit$evaluation_log
  log$eta = e
  eta_log0 = rbind(eta_log0,log,use.names=FALSE)
}

lfl.eta0 = ggplot(data = eta_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "LFL Learning Rate Tuning", subtitle="max_depth=6",color='eta') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,4,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.eta0

######### Max_Depth Tune
max_depth_log0 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  lfl.fit = xgb.train(params = params, data = dtrain0, nrounds = 1000, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=lfl.fit$evaluation_log
  log$max_depth = d
  max_depth_log0 = rbind(max_depth_log0,log,use.names=FALSE)
}

lfl.depth0 = ggplot(data = max_depth_log0,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "LFL Interaction Depth Tuning", subtitle="eta=0.1",color='max_depth') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,4,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.depth0

######### Best0
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
lfl.fit0 = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 10, early_stop_round = 10, maximize = F)

######### Feature Importance Plot
names = colnames(all_LFL[18:811])
importance_matrix = xgb.importance(names, model = lfl.fit0)
importance_matrix
#write.csv(importance_matrix,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/Importance Matrix_LFL.csv", row.names = FALSE)
xgb.ggplot.importance(importance_matrix[1:10,])+
  labs(title = "LFL Descriptor Importance") 
feature = importance_matrix$Feature[1:5]

######### Data Post Process
train1 = train0[,feature]
test1 = test0[,feature]

######### Correalation Plot

cor = cbind(all_LFL$LFL,log(all_LFL$LFL),all_LFL[,feature])
colnames(cor) = c("LFL","log(LFL)","AMID_C","Mor06p","ATS0p","TASA","ATS0v")

pairs.panels(cor, 
             method = "pearson",
             hist.col = brewer.pal(9,"Blues")[8],
             density = TRUE,
             ellipses = TRUE,
             cor = FALSE
)

#tab_train = train1
#tab_test = test1
#tab_train$'log(LFL_exp)' = labels
#tab_test$'log(LFL_exp)' = labels1
#tab_train = cbind(train[,1:17],tab_train)
#tab_test = cbind(test[,1:17],tab_test)
#write.csv(tab_train,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_train_LFL.csv", row.names = FALSE)
#write.csv(tab_test,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture FL/Data/tab_test_LFL.csv", row.names = FALSE)

dtrain1 = xgb.DMatrix(data = data.matrix(train1),label = labels) 
dtest1 = xgb.DMatrix(data = data.matrix(test1),label = labels1)

######### eta Tune
eta_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),eta=integer()) 
E = seq(0.1,0.5,0.1)
for(e in E){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=e, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
  lfl.fit = xgb.train(params = params, data = dtrain1, nrounds = 1000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=lfl.fit$evaluation_log
  log$eta = e
  eta_log1 = rbind(eta_log1,log,use.names=FALSE)
}

lfl.eta1 = ggplot(data = eta_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(eta))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "LFL Learning Rate Tuning", subtitle="max_depth=10",color='eta') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,1000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.eta1

######### Max_Depth Tune
max_depth_log1 = data.frame(Iteration=integer(),Test_Err=double(),Train_Err=double(),max_depth=integer()) 
for(d in 2:10){
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.2, gamma=0, max_depth=d, min_child_weight=1, subsample=1, colsample_bytree=1)
  lfl.fit = xgb.train(params = params, data = dtrain1, nrounds = 1000, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 1000, early_stop_round = 10, maximize = F)
  log=lfl.fit$evaluation_log
  log$max_depth = d
  max_depth_log1 = rbind(max_depth_log1,log,use.names=FALSE)
}

lfl.depth1 = ggplot(data = max_depth_log1,mapping = aes(x = Iteration,y = Test_Err,color=factor(max_depth))) + 
  geom_line(size = 1,alpha = 1,linetype = 1) +
  labs(x = "Iteration",y = "Test RMSE",title = "LFL Interaction Depth Tuning", subtitle="eta=0.2",color='max_depth') +
  scale_y_continuous(limits = c(0,0.5),breaks=seq(0,8,0.05)) +
  scale_x_continuous(limits = c(0,1000),breaks=seq(0,3000,200)) +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.depth1

######### Best1
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.2, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
lfl.fit1 = xgb.train(params = params, data = dtrain1, nrounds = 81, watchlist = list(val=dtest1,train=dtrain1), print_every_n = 5, early_stop_round = 10, maximize = F)

lfl.pred.train = predict(lfl.fit1,newdata = dtrain1)
#tab_train$LFL_pred = lfl.pred.train 
lfl.err.train = lfl.pred.train - log(train$LFL)
#tab_train$LFL_err = lfl.err.train
x = rep(0,length(lfl.pred.train))
lfl.pred.train = data.frame(x,lfl.pred.train,lfl.err.train,train$LFL)
colnames(lfl.pred.train) = c("x", "lfl.pred","lfl.err","LFL")

lfl.pred.test = predict(lfl.fit1,newdata = dtest1)
#tab_test$LFL_pred = lfl.pred.test
lfl.err.test = lfl.pred.test - log(test$LFL)
#tab_test$LFL_err = lfl.err.test
x = rep(1,length(lfl.pred.test))
lfl.pred.test = data.frame(x,lfl.pred.test,lfl.err.test,test$LFL)
colnames(lfl.pred.test) = c("x", "lfl.pred","lfl.err","LFL")

lfl.pred = rbind(lfl.pred.train,lfl.pred.test)

#########Plot
lfl = ggplot(data = lfl.pred,mapping = aes(x = log(LFL),y = lfl.pred,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_abline(alpha = 0.8) +
  labs(x = expression(log('LFL')[exp]),y = expression(log('LFL')[pred]),title = "LFL Model Performance") +
  scale_y_continuous(limits = c(0,3),breaks=seq(0,45,0.5)) +
  scale_x_continuous(limits = c(0,3),breaks=seq(0,45,0.5)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=203','Test Set n=68')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=203','Test Set n=68')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

lfl

lfl.res = ggplot(data = lfl.pred,mapping = aes(x = log(LFL),y = lfl.err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  geom_hline(aes(yintercept=-0.2),alpha=0.3) +
  geom_hline(aes(yintercept=+0.2),alpha=0.3) +
  labs(x = expression(log('LFL')[exp]),y = "Residual",title = "Residual Plot") +
  scale_x_continuous(limits = c(0,3),breaks=seq(0,45,0.5)) +
  scale_y_continuous(limits = c(-0.4,0.4),breaks=seq(-3,3,0.1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=203','Test Set n=68')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=203','Test Set n=68')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

lfl.res

######### Willanms Plot

lfl.pred.train = predict(lfl.fit1,newdata = dtrain1)
lfl.pred.test = predict(lfl.fit1,newdata = dtest1)
pred.all = c(lfl.pred.train,lfl.pred.test)
avg = mean(pred.all)
std_err = sqrt(sd(lfl.err.test))
x = rep(0,length(lfl.pred.train))
err.train = data.frame(x,lfl.pred.train,lfl.err.train/std_err)
x = rep(1,length(lfl.pred.test))
err.test = data.frame(x,lfl.pred.test,lfl.err.test/std_err)
err.train$leverage0 = (err.train$lfl.pred.train - avg)^2
err.train$leverage = 1/length(pred.all)+err.train$leverage0/sum(err.train$leverage0)
err.test$leverage0 = (err.test$lfl.pred.test - avg)^2
err.test$leverage = 1/length(pred.all)+err.test$leverage0/sum(err.test$leverage0)
names(err.test) =names(err.train)
error = rbind(err.train,err.test) 


lfl.wil = ggplot(data = error,mapping = aes(x = leverage,y = lfl.err.train.std_err,color = factor(x))) +
  geom_point(size = 2,aes(shape = factor(x)),alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.3) +
  geom_hline(aes(yintercept=-3),alpha=0.8) +
  geom_hline(aes(yintercept=3),alpha=0.8) +
  geom_vline(aes(xintercept=3*7/203),alpha=0.8) +
  labs(x = "Leverage",y = "Standard Residuals",title = "LFL Williams Plot") +
  scale_x_continuous(limits = c(0,0.125),breaks=seq(0,45,0.025)) +
  scale_y_continuous(limits = c(-4,4),breaks=seq(-4,4,1)) +
  scale_color_manual(values=c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"PuBuGn")[8]),labels = c('Training Set n=203','Test Set n=68')) +
  scale_shape_manual(values = c(16,17),labels = c('Training Set n=203','Test Set n=68')) +
  theme(
    plot.title = element_text(face = "bold"),  
    legend.title=element_blank(),
  )

lfl.wil

######### Evaluation
lfl.r2.train = cor(lfl.pred.train$lfl.pred,log(lfl.pred.train$`LFL`))^2
lfl.r2.test = cor(lfl.pred.test$lfl.pred,log(lfl.pred.test$`LFL`))^2
lfl.r2.all = cor(lfl.pred$lfl.pred,log(lfl.pred$`LFL`))^2

lfl.rmse.train = rmse(lfl.pred.train$lfl.err)
lfl.rmse.test = rmse(lfl.pred.test$lfl.err)
lfl.rmse.all = rmse(lfl.pred$lfl.err)

lfl.aae.train = aae(lfl.pred.train$lfl.err)
lfl.aae.test = aae(lfl.pred.test$lfl.err)
lfl.aae.all = aae(lfl.pred$lfl.err)

lfl.loo = q(lfl.pred.train,mean(labels),lfl.err.train)
lfl.ext = q(lfl.pred.test,mean(labels),lfl.err.test)

lfl.value = c(lfl.rmse.train,lfl.rmse.test,lfl.rmse.all,lfl.r2.train,lfl.r2.test,lfl.r2.all,
              lfl.aae.train,lfl.aae.test,lfl.aae.all,lfl.loo,lfl.ext)

names(lfl.value) = c("RMSE.Train", "RMSE.Test","RMSE.All","R^2.Train","R^2.Test","R^2.All",
                     "AAE.Train","AAE.Test","AAE.All","Q^2_loo","Q^2_ext")

lfl.value
