library(dplyr)
library(caTools)

########### Data Preprocess
desc_FP = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FP/Descriptor_FP.csv")
data_FP = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FP/FP_Data.csv")
name_FP = desc_FP$name
desc_FP = desc_FP[,-1]
desc_FP = desc_FP[, which(colSums(desc_FP) != 0)]
desc_FP = cbind(name_FP,desc_FP)
data1_FP = as.matrix(data_FP[,2:47])
desc1_FP = as.matrix(desc_FP[,2:1090])
all_FP = as.data.frame(data1_FP%*%desc1_FP)
all_FP = cbind(data_FP[,1:48],all_FP)
write.csv(all_FP,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FP/FP_Complete.csv", row.names = FALSE)

###########
all_FP = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FP/FP_Complete.csv")
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

all_FP = all_FP[sample(nrow(all_FP)),]
#all_FP1 = all_FP[,-c(1:14)]
feature = c("Mor10v","NsCH3", "MATS2dv","Mor10m","SIC5")
all_FP1 = all_FP[,feature]
folds = cut(seq(1,nrow(all_FP)),breaks = 5,labels = FALSE)

###########Supplementary
#Step 1
all_FP2 = cbind(all_FP[,1:48],all_FP[,feature])
all_FP2$Fold = folds
#Step 3
all_FP2 = all_FP2[order(all_FP2$X),]
write.csv(all_FP2,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FP/FP_Sup.csv", row.names = FALSE)

sample = sample.split(all_FP$X, SplitRatio = .75)
train = subset(all_FP, sample == TRUE)
test  = subset(all_FP, sample == FALSE)
