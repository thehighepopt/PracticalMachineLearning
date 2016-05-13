#You should create a report describing how you built your model, 
#how you used cross validation, 
#what you think the expected out of sample error is, 
#and why you made the choices you did. 
#You will also use your prediction model to predict 20 different test cases.



training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

dim(training)
head(training[,1:10])
str(training$classe)
mod1 <- train(classe ~ .,method="rf",data=training)
mod2 <- train(classe ~ user_name + roll_belt + pitch_belt +yaw_belt + roll_arm + pitch_arm
              + yaw_arm + roll_dumbell + pitch_dumbell + yaw_dumbell,method="rf",data=training)


