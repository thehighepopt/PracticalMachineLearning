data("faithful")
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)

or

modFit <- train(eruptions ~ waiting, data=trainFaith,method="lm")
summary(modFit$finalModel)


data(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")
qplot(age,wage, color=jobclass,data=training)

modFit <- train(wage ~ age + jobclass + education,
                method = "lm", data=training)
finMod <- modFit$finalModel
plot(finMod, 1, pch=19,cex=0.5)
qplot(finMod$fitted,finMod$residuals, colour=race,data=training)
plot(finMod$residuals,pch=19)
pred <- predict(modFit,testing)
qplot(wage,pred,colour=year,data=testing)

index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
    theme_bw()

cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)

cols <- grep(glob2rx("IL*"),names(training))
b <- training[,cols]

g <- preProcess(b,method = "pca", thresh = .80)
traing <- predict(g,b)


##Quiz2 Q5
pred1 <- train(diagnosis ~ .,method ="glm", data=b)
predictions <- predict(pred1, newdata = testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
A1 <- C1$overall[1]

pred2 <- train(diagnosis ~ .,method ="glm",preProcess = "pca", data=b, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
predictions2 <- predict(pred2, newdata = testing)
C2 <- confusionMatrix(predictions, testing$diagnosis)
A2 <- C2$overall[1]

##Week 3
data("iris")
inTrain <- createDataPartition(y=iris$Species,p=0.7,list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training) ; dim(testing)
qplot(Petal.Width,Sepal.Width,color=Species,data=training)
modFit <- train(Species ~ ., method="rpart",data=training)
plot(modFit$finalModel, uniform=TRUE,main="Classifcation Tree")
text(modFit$finalModel,use.n=TRUE, all=TRUE, cex=.8)
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit,newdata=testing)

###Bagging
ll <- matrix(NA,nrow=10,nco=155)
for(i in 1:10) {
    ss <- sample(1:dim(ozone)[1],replace = TRUE)
    ozone0 <- ozone[ss,]
    onone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(tempurature ~ ozone,data=ozone0,span=0.2)
    ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature, pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)
ctreeBag$fit()

###Random Forest
##very effective
data(iris)
library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modFit <- train(Species ~ .,data=training,method="rf",prox=TRUE)
modFit
getTree(modFit$finalModel,k=2)

irisP <- classCenter(training[,c(3,4)], training$Species,modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5,shape=4,data=irisP)
pred <- predict(modFit,testing)
testing$predRight <- pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")

###Boosting
###also very accurate
library(ISLR)
data(Wage)
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~ .,method="gbm",data=training,verbose=FALSE)
qplot(predict(modFit,testing),wage,data=testing)

##model based prediction
inTrain <- createDataPartition(y=iris$Species,p=0.7,list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modlda <- train(Species ~ .,data=training,method="lda")
modnb <- train(Species ~ .,data=training,method="nb")
plda <- predict(modlda,testing)
pnb <- predict(modnb,testing)
table(plda,pnb)
equalPreds <- (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour = equalPreds,data=testing)

##Quiz 3
##1
library(rpart)
inTrain <- createDataPartition(y=segmentationOriginal$Case,p=0.7,list = FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]
set.seed(125)
modFit1 <- train(Class ~ ., data = training, method = "rpart")
fancyRpartPlot(modFit1$finalModel)

##3
library(pgmm)
data(olive)
olive = olive[,-1]
inTrain <- createDataPartition(y=olive$Area,p=0.7,list = FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
modFit3 <- train(Area ~ ., data = training, method = "rpart")
fancyRpartPlot(modFit3$finalModel)
pred <- predict(modFit3,as.data.frame(t(colMeans(olive))))

##4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
trainSA$chd <- as.factor(trainSA$chd)
testSA = SAheart[-train,]
set.seed(13234)
modFit4 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA,method = "glm",
                 family = "binomial")
predtest <- as.integer(predict(modFit4,newdata = testSA))
predtrain <- as.integer(predict(modFit4,newdata = trainSA))
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA[,10],predtest)
missClass(trainSA[,10],predtrain)

##5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)
modFit5 <- train(y ~ ., data=vowel.train,method = "rf")
varImp(modFit5)

###Week 4
data(Wage)
wage <- subset(Wage,select = -c(logwage))
inBuild <- createDataPartition(y=Wage$wage,p=0.7,list = FALSE)
 validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]

mod1 <- train(wage ~ .,method = "glm",training)
mod2 <- train(wage ~ ., method ="rf",data = training,trControl = trainControl(method = "cv"),number=3)
pred1 <- predict(mod1,testing)
pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)

predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~., method="gam",data=predDF)
combPred <- predict(combModFit,predDF)
#test errors
sqrt(sum((pred1-testing$wage)^2))

pred1V <- predict(mod1,validation)
pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

from.dat <- as.Date("01/01/08", format = "%m/%d/%y")
to.dat <- as.Date("12/31/13", format = "%m/%d/%y")
getSymbols("GOOG", src="google",from=from.dat,to = to.dat)
mGoog <- to.monthly(mGoog)
googOpen <- Op(mGoog)
ts1 <- ts(goodOpen,frequency=12)
plot(ts1,xlab="Years+1",ylab="GOOG")
plot(decompose(ts1,xlab="Years+1"))

ts1Train <- window(ts1,start=1,end=5)
ts1Test <- windo(ts1,start=5,end=(7-0.01))
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")
##exponential smoothing
ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test,col='red')

#train on Iris
kmeans1 <- kmeans(subset(training,select = -c(Species)),centers=3)
training$clusters <- as.factor(kmeans1$cluster)
table(kmeans1$cluster,training$Species)
modFit <- train(clusters ~ .,data=subset(training,selet = -c(Species)),method="rpart")
table(predict(modFit,training),training$Species)

###Quiz 4
##1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
modfit1 <- train(y ~ .,method="rf",data = vowel.train)
modfit2 <- train(y ~ .,data = vowel.train,method="gbm",verbose=FALSE)
pred1 <- predict(modfit1,vowel.test)
pred2 <- predict(modfit2,vowel.test,verbose=FALSE)
cfnm1 <- confusionMatrix(pred1,vowel.test$y)
cfnm2 <- confusionMatrix(pred2,vowel.test$y)
cfnm1$overall['Accuracy']
cfnm2$overall['Accuracy']
combo <- data.frame(pred1,pred2,y=vowel.test$y)
modcomb <- train(y ~., combo)
predcomb <- predict(modcomb,vowel.test)
cfnmcomb <- confusionMatrix(predcomb,vowel.test$y)
cfnmcomb$overall['Accuracy']


#2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

modfitrf <- train(diagnosis ~ .,method="rf",data = training)
modfitgbm <- train(diagnosis ~ .,method="gbm",data = training,verbose=FALSE)
modfitlda <- train(diagnosis ~ .,method="lda",data = training)
predDF <- data.frame(predrf,predgbm,predlda,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~., method="rf",data=predDF)

predrf <- predict(modfitrf,testing)
predgbm <- predict(modfitgbm,testing)
predlda <- predict(modfitlda,testing)
##predall <- data.frame(pred1=predrf,pred2=predgbm,pred3 = predlda, pred4 = combModFit)
combPred <- predict(combModFit,testing)
cmrf <- confusionMatrix(predrf,testing$diagnosis)
cmgbm <- confusionMatrix(predgbm,testing$diagnosis)
cmlda <- confusionMatrix(predlda,testing$diagnosis)
cmcomb <- confusionMatrix(combPred,testing$diagnosis)
cmrf$overall['Accuracy']
cmgbm$overall['Accuracy']
cmlda$overall['Accuracy']
cmcomb$overall['Accuracy']



#3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

modfitl <- train(CompressiveStrength ~ .,method = "lasso", data = training)
plot.enet(modfitl,xvar = "penalty",use.color = TRUE)
plot.enet(modfitl$finalModel,xvar = "penalty",use.color = TRUE)

#4
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(mdy(dat$date)) < 2012,]
testing = dat[year(mdy(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
plot(training$visitsTumblr)
modfitfc <- bats(tstrain)
fcast <- forecast.bats(modfitfc, h=nrow(testing),level=95)
plot(fcast)
lines(testing$visitsTumblr,col="red")
accuracy(fcast,testing$visitsTumblr)
tab <- table ((testing$visitsTumblr>fcast$lower) & 
           (testing$visitsTumblr<fcast$upper))

tab[2]/nrow(testing) 

#5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
modsvm <- svm(CompressiveStrength ~ .,data = training)
predsvm <- predict(modsvm,testing)









