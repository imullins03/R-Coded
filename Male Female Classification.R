data <- read.csv("C:/Users/imull/Documents/Davis.csv")
data$Entry = NULL
attach(data)
sex <- ifelse(data$sex == "M", 0, 1)
sex <- as.factor(sex)
colSums(is.na(data))
repwtM <- mean(repwt...kg, na.rm = T)
repht <- mean(repht...cm, na.rm = T)
library(na.tools)
repwt...kg[is.na(repwt...kg)] <- mean(repwt...kg, na.rm = T)
repwt...kg
repht...cm[is.na(repht...cm)] <- mean(repht...cm, na.rm = T)
repht...cm
sum(is.na(repwt...kg))
sum(is.na(repht...cm))
data2 <- data.frame(sex, height, weight, repwt...kg, repht...cm)

set.seed(100)
trainingindex <- sample(1:nrow(data2), .75*nrow(data2))
train <- data[trainingindex,]
test <- data[-trainingindex,]

y <- as.matrix(sex)
x <- as.matrix(data.table(height, weight, repwt...kg, repht...cm))
x.train <- x[trainingindex,]
y.train <- y[trainingindex]
x.test <- x[-trainingindex,]
y.test <- y[-trainingindex]

LogModel <- glm(sex~., family = "binomial", data = train)
summary(LogModel)

premodel <- predict(LogModel, newdata = test, type = "response")
summary(premodel)

CMatrix <- table(test$sex,premodel > 0.5)
CMatrix
CM <- confusionMatrix(table(premodel, test$sex))

LogAcc <- (CMatrix[[1,1]] + CMatrix[[2,2]])/sum(CMatrix) #Accuracy
LogAcc
LogMisC <- (CMatrix[[1,2]] + CMatrix[[2,1]])/sum(CMatrix) #Misclassification Error
LogMisC
LogSen <- CMatrix[[2,2]]/sum((CMatrix[[2,2]] + CMatrix[[2,1]])) #Sensitivity/Recall/True Positive
LogSen
LogSpe <- CMatrix[[1,1]]/sum(CMatrix[[1,1]] + CMatrix[[1,2]]) #Specificity/True Negative
LogSpe
LogPre <- CMatrix[[2,2]]/sum(CMatrix[[2,2]] + CMatrix[[1,2]]) #Precision
LogPre
LogPrev <- CMatrix[[2,2]]/sum(CMatrix) #Prevalance
LogPrev
LogT1E <- CMatrix[[2,1]]/sum(CMatrix[[2,1]] + CMatrix[[2,2]]) #Type 1 Error
LogT1E
LogT2E <- CMatrix[[1,2]]/sum(CMatrix[[1,1]] + CMatrix[[1,2]]) #Type 2 Error
LogT2E

roc(test$sex, premodel, plot = TRUE)
roc.info <- roc(test$sex, premodel, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)
roc.df <- data.frame(TPP = roc.info$sensitivities*100, 
                     FPP = (1 - roc.info$sensitivities)*100, Thresholds = roc.info$thresholds)


#Logistic Regression with 10 k Cross Validation
LogModel2 <- cv.glmnet(x.train, y.train, type.measure = "deviance", 
                       nfolds = 10, alpha = .05, family = "binomial")
LogModel2
summary(LogModel2)

premodel2 <- predict(LogModel2, newx = x.test, s = LogModel2$lambda.1se)
summary(premodel2)
CMatrix2 <- table(y.test, premodel2 > 0.1705)
CMatrix2
CVLogAcc <- (CMatrix2[[1,1]] + CMatrix2[[2,2]])/sum(CMatrix2)
CVLogAcc

LogModel3 <- cv.glmnet(x.train, y.train, type.measure = "deviance", 
                       nfolds = 5, alpha = .05, family = "binomial")

premodel3 <- predict(LogModel3, newx = x.test, s = LogModel3$lambda.1se)
summary(premodel3)
CMatrix3 <- table(y.test, premodel3 > 0.19338)
CVLogAcc3 <- (CMatrix3[[1,1]] + CMatrix3[[2,2]])/sum(CMatrix3)
CVLogAcc3

roc(y.test, premodel2, plot = TRUE)
roc.info2 <- roc(y.test, premodel2, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)
plot.roc(y.test, premodel2, plot = TRUE, add = TRUE)

plot(LogModel3)
plot(LogModel2)
