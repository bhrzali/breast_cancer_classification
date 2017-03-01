
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


