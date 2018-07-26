
breast_data = read.csv("datasets/breast-cancer-wisconsin.csv",header=F,na.strings="?")
breast_data = na.omit(breast_data)
#Removing the first column. It contains the sample code number
breast_data = breast_data[,-1]
#Making the final attribute qualitative
breast_data$V11 = as.factor(breast_data$V11)
#The names of the columns of the attributes are arranged in the same order as above.
names(breast_data)

nrow(breast_data)

set.seed(1)
#creating a training dataset
train = sample(nrow(breast_data),550)

glm.fit = glm(V11~.,data=breast_data,family=binomial,subset=train)
glm.prob = predict(glm.fit,newdata=breast_data[-train,],type="response")
glm.pred = rep(2,length(glm.prob))
glm.pred[glm.prob>0.5]=4

#error rate
mean(glm.pred!=breast_data[-train,]$V11)*100

#accuracy
mean(glm.pred==breast_data[-train,]$V11)*100

#confusion table 2=benign 4=malignant
table(glm.pred,breast_data[-train,]$V11)

91/(91+1)*100

39/(39+2)*100

library(MASS)
lda.fit = lda(V11~.,data=breast_data[train,])
lda.pred = predict(lda.fit,newdata=breast_data[-train,])

#error rate
mean(lda.pred$class!=breast_data[-train,]$V11)*100

#accuracy
mean(lda.pred$class==breast_data[-train,]$V11)*100

table(lda.pred$class,breast_data[-train,]$V11)

92/92*100

38/(38+3)*100

library(class)
train.Y = breast_data[train,]$V11
train_data = breast_data[train,-ncol(breast_data)]
test_data = breast_data[-train,-ncol(breast_data)]
test.Y = breast_data[-train,]$V11
#knn method with k=4
knn.pred = knn(train_data,test_data,train.Y,k=4)

##error rate
mean(knn.pred!=test.Y)*100

##accuracy
mean(knn.pred==test.Y)*100

table(knn.pred,test.Y)

40/(40+1)*100

90/(90+2)*100


