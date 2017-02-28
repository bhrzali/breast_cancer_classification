
breast_data = read.csv("breast-cancer-wisconsin.csv",header=F,na.strings="?")
breast_data = na.omit(breast_data)
breast_data = breast_data[,-1]
names(breast_data)

nrow(breast_data)

breast_data$V11 = as.factor(breast_data$V11)
set.seed(1)
train = sample(nrow(breast_data),550)
glm.fit = glm(V11~.,data=breast_data,family=binomial,subset=train)
glm.prob = predict(glm.fit,newdata=breast_data[-train,],type="response")
glm.pred = rep(2,length(glm.prob))
glm.pred[glm.prob>0.5]=4
mean(glm.pred!=breast_data[-train,]$V11)*100

#accuracy
mean(glm.pred==breast_data[-train,]$V11)*100


