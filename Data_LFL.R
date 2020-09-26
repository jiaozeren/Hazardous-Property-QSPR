library(dplyr)
library(caTools)

########### Data Preprocess
desc_LFL = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/Descriptor_LFL.csv")
data_LFL = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL_Data_LFL.csv")
name_LFL = desc_LFL$name
desc_LFL = desc_LFL[,-1]
desc_LFL = desc_LFL[, which(colSums(desc_LFL) != 0)]
desc_LFL = cbind(name_LFL,desc_LFL)
data1_LFL = as.matrix(data_LFL[,2:16])
desc1_LFL = as.matrix(desc_LFL[,2:795])
all_LFL = as.data.frame(data1_LFL%*%desc1_LFL)
all_LFL = cbind(data_LFL,all_LFL)
write.csv(all_LFL,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL_Complete_LFL.csv", row.names = FALSE)

###########
all_LFL = read.csv("/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL/FL_Complete_LFL.csv")
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

all_LFL = all_LFL[sample(nrow(all_LFL)),]
#all_LFL1 = all_LFL[,-c(1:17)]
feature = c("AMID_C","Mor06p","ATS0p","TASA","ATS0v" )
all_LFL1 = all_LFL[,feature]
folds = cut(seq(1,nrow(all_LFL)),breaks = 5,labels = FALSE)

###########Supplementary
#Step 1
all_LFL2 = cbind(all_LFL[,1:17],all_LFL[,feature])
all_LFL2$Fold = folds
#Step 3
all_LFL2 = all_LFL2[order(all_LFL2$X),]
write.csv(all_LFL2,"/Volumes/GoogleDrive/我的云端硬盘/QSPR/Mixture/Data/FL/LFL_Sup.csv", row.names = FALSE)


sample = sample.split(all_LFL$X, SplitRatio = .75)
train = subset(all_LFL, sample == TRUE)
test  = subset(all_LFL, sample == FALSE)
