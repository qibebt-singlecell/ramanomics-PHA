# by zx @qibebt.ac.cn
#
#
#----------------------------------------------------------------------------------------
library(e1071)
library(pROC) 

setwd('.data\\PHA\\data1\\250102model') 
wine <- read.table('BDC2_ZWQ_base_gyh_dt3+34hb.txt',header = T)

table(wine$class)

train_sub = sample(nrow(wine),7/10*nrow(wine))
traindata = wine[train_sub,]
table(traindata$class)
#write.table(traindata, file = "ABD3-4HB_ZWQ_base_gyh800.txt", sep = "\t", row.names = FALSE, col.names = T)
testdata = wine[-train_sub,]
table(testdata$class)

traindata$class = as.factor(traindata$class)
testdata$class = as.factor(testdata$class)

###-------------Hyperparameter validation-------------

library(e1071)
#  tune.svm 
set.seed(123)
tune_result <- tune.svm(class ~ .,
                        data = traindata,
                        type = "C",
                        kernel = "radial",
                    
                        cost = c(0.01, 0.1, 1, 10, 100),
                        gamma = c(0.001, 0.01, 0.1, 1))

print(tune_result$best.parameters)

best_model <- tune_result$best.model

###-------------Hyperparameter validation-------------

#svm：C-classification
wine_svm <- svm(class~., data = traindata,type = 'C',kernel = 'radial')
summary(wine_svm)

#
confusion.train.svm=table(traindata$class,predict(wine_svm,traindata,type="class"))
accuracy.train.svm=sum(diag(confusion.train.svm))/sum(confusion.train.svm)
confusion.train.svm
accuracy.train.svm
#
confusion.test.svm=table(testdata$class,predict(wine_svm,testdata,type="class"))
accuracy.test.svm=sum(diag(confusion.test.svm))/sum(confusion.test.svm)
confusion.test.svm
accuracy.test.svm


# TEST
setwd('.data\\PHA\\data1\\250102model') #\\testdata
test_data = read.table('C13_ZWQ_base_gyh_dt.txt',header = T, row.names = 1) 
test_data$class <- '3HB-4HB'
table(test_data$class)
#wine <- wine[,-674]
test_data <- test_data[,-673]
colnames(test_data) <- colnames(wine)

confusion.test.svm=table(test_data$class,predict(wine_svm,test_data,type="class"))
accuracy.test.svm=sum(diag(confusion.test.svm))/sum(confusion.test.svm)
confusion.test.svm
accuracy.test.svm
#
test_data$predict <- predict(wine_svm,test_data,type="class")
write.table(test_data, file = "C13_ZWQ_base_gyh_pre0113.txt", sep = "\t", row.names = T, col.names = T)
