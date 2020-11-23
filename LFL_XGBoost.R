library(xgboost)
library(ggplot2)
library(RColorBrewer)

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
  
  #XGBoost Regression Training
  #Use Tuned Hyper-parameters, tuning can be found below
  params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.2, gamma=0, max_depth=10, min_child_weight=1, subsample=1, colsample_bytree=1)
  lfl.fit = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 1000, early_stop_round = 10, maximize = F)
  
  #Prediction
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

###########Supplementary Table
#Step 2
all_LFL2$exp = lfl.all$exp
all_LFL2$pred = lfl.all$pred
all_LFL2$err = lfl.all$err

###########Performance Plot
#Prediction vs. Experimental
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

#Residual Plot
lfl.res = ggplot(data = lfl.all,mapping = aes(x = exp,y = err,color = factor(x))) +
  geom_point(size = 2,alpha = 1) +
  geom_hline(aes(yintercept=0),alpha=0.8) +
  labs(x = expression(log('LFL')[exp]),y = "Residual",title = "Residual Plot",
       subtitle = "Database Size = 271, Fold Size = 55") +
  scale_x_continuous(limits = c(0.5,3),breaks=seq(0,45,0.5)) +
  scale_y_continuous(limits = c(-1,1),breaks=seq(-3,3,0.2)) +
  scale_color_manual(values=brewer.pal(5,"Paired"),labels = c('1','2','3','4','5'),name ="Fold") +
  theme(
    plot.title = element_text(face = "bold"),  
  )

lfl.res

#Williams Plot
avg = mean(lfl.all$pred)
std_err = sd(lfl.all$err)*2
lfl.all$std = std_err
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

###########Test Statistics
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

###########Parameter Tuning
#eta Tune
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

#Max_Depth Tune
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

#Best Hyper-parameters Fit
params = list(booster = "gbtree",objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
lfl.fit0 = xgb.train(params = params, data = dtrain0, nrounds = 200, watchlist = list(val=dtest0,train=dtrain0), print_every_n = 10, early_stop_round = 10, maximize = F)

#Feature Importance Plot/Descriptor Ranking
names = colnames(all_LFL[18:811])
importance_matrix = xgb.importance(names, model = lfl.fit0)
importance_matrix
xgb.ggplot.importance(importance_matrix[1:10,])+
  labs(title = "LFL Descriptor Importance") 
feature = importance_matrix$Feature[1:5]