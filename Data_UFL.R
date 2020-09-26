library(dplyr)
library(caTools)

########### Data Preprocess
desc_UFL = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/Descriptor_UFL.csv")
data_UFL = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL_Data_UFL.csv")
name_UFL = desc_UFL$name
desc_UFL = desc_UFL[,-1]
desc_UFL = desc_UFL[, which(colSums(desc_UFL) != 0)]
desc_UFL = cbind(name_UFL,desc_UFL)
data1_UFL = as.matrix(data_UFL[,2:13])
desc1_UFL = as.matrix(desc_UFL[,2:795])
all_UFL = as.data.frame(data1_UFL%*%desc1_UFL)
all_UFL = cbind(data_UFL,all_UFL)
write.csv(all_UFL,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL_Complete_UFL.csv", row.names = FALSE)

###########
all_UFL = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL/FL_Complete_UFL.csv")
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

all_UFL = all_UFL[sample(nrow(all_UFL)),]
#all_UFL1 = all_UFL[,-c(1:14)]
feature = c("Mor21p","SMR_VSA5","Mor18","AETA_beta_ns","GeomShapeIndex","ETA_dBeta")
all_UFL1 = all_UFL[,feature]
folds = cut(seq(1,nrow(all_UFL)),breaks = 5,labels = FALSE)

###########Supplementary
#Step 1
all_UFL2 = cbind(all_UFL[,1:14],all_UFL[,feature])
all_UFL2$Fold = folds
#Step 3
all_UFL2 = all_UFL2[order(all_UFL2$X),]
write.csv(all_UFL2,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL/UFL_Sup.csv", row.names = FALSE)


sample = sample.split(all_UFL$X, SplitRatio = .75)
train = subset(all_UFL, sample == TRUE)
test  = subset(all_UFL, sample == FALSE)
