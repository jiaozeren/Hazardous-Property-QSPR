library(dplyr)
library(caTools)

###########Data Preprocess
#Combine Descriptor csv file with label csv file
desc_LFL = read.csv("/File Location")
data_LFL = read.csv("/File Location")
name_LFL = desc_LFL$name
desc_LFL = desc_LFL[,-1]
desc_LFL = desc_LFL[, which(colSums(desc_LFL) != 0)]
desc_LFL = cbind(name_LFL,desc_LFL)
data1_LFL = as.matrix(data_LFL[,2:16])
desc1_LFL = as.matrix(desc_LFL[,2:795])
all_LFL = as.data.frame(data1_LFL%*%desc1_LFL)
all_LFL = cbind(data_LFL,all_LFL)
write.csv(all_LFL,"/Save Location", row.names = FALSE)

###########Read Data
all_LFL = read.csv("/File Location")
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

###########Generate Subset of Data Based on Selected Descriptor
feature = c("AMID_C","Mor06p","ATS0p","TASA","ATS0v" )
all_LFL = all_LFL[,feature]

###########Implement 5-Fold CV
all_LFL = all_LFL[sample(nrow(all_LFL)),]
folds = cut(seq(1,nrow(all_LFL)),breaks = 5,labels = FALSE)

###########Regular Train Test Split
sample = sample.split(all_LFL$X, SplitRatio = .75)
train = subset(all_LFL, sample == TRUE)
test  = subset(all_LFL, sample == FALSE)

###########Supplementary Table Generation
#Step 1
all_LFL2 = cbind(all_LFL[,1:17],all_LFL[,feature])
all_LFL2$Fold = folds

#Step 3
all_LFL2 = all_LFL2[order(all_LFL2$X),]
write.csv(all_LFL2,"/File Location", row.names = FALSE)
