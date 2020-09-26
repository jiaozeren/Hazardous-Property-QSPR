library(dplyr)
library(caTools)

########### Data Preprocess
desc_AIT = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/AIT/Descriptor_AIT.csv")
data_AIT = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/AIT/AIT_Data.csv")
name_AIT = desc_AIT$name
desc_AIT = desc_AIT[,-1]
desc_AIT = desc_AIT[, which(colSums(desc_AIT) != 0)]
desc_AIT = cbind(name_AIT,desc_AIT)
data1_AIT = as.matrix(data_AIT[,2:20])
desc1_AIT = as.matrix(desc_AIT[,2:1036])
all_AIT = as.data.frame(data1_AIT%*%desc1_AIT)
all_AIT = cbind(data_AIT,all_AIT)
write.csv(all_AIT,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/AIT/AIT_Complete.csv", row.names = FALSE)

###########
all_AIT = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/AIT/AIT_Complete.csv")
set.seed(121)

#RMSE Function
rmse = function(error)
{
  sqrt(mean(error^2))
}
#AAE Function
aae = function(error)
{
  mean(abs(error))
}
#Q Function
q = function(pred,avg,error)
{
  1-(sum(error^2)/sum((pred-avg)^2))
}

all_AIT = all_AIT[sample(nrow(all_AIT)),]
#all_AIT1 = all_AIT[,-c(1:14)]
feature = c("NssCH2","Mor17m","ATSC2Z","Mor10v")
all_AIT1 = all_AIT[,feature]
folds = cut(seq(1,nrow(all_AIT)),breaks = 5,labels = FALSE)

###########Supplementary
#Step 1
all_AIT2 = cbind(all_AIT[,1:21],all_AIT[,feature])
all_AIT2$Fold = folds
#Step 3
all_AIT2 = all_AIT2[order(all_AIT2$X),]
write.csv(all_AIT2,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/AIT/AIT_Sup.csv", row.names = FALSE)


sample = sample.split(all_AIT$X, SplitRatio = .75)
train = subset(all_AIT, sample == TRUE)
test  = subset(all_AIT, sample == FALSE)
