# Question 1.
library(AppliedPredictiveModeling); data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list = FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2. 
data("concrete")
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain, ]
head(training, 10)
testing = mixtures[-inTrain, ]
library(ggplot2); library(Hmisc); library(dplyr)
# using the cut2() function: it might be useful to break up a continuous 
# variable such as training into a categorical variable. 
training <- mutate(training, index = 1:nrow(training))
breaks <- 10
cutIndex <- cut2(training$index, g = breaks)
qplot(index, CompressiveStrength, data = training, color = cut2(training$Age, g = breaks))


# Question 3. 
hist(training$Superplasticizer, breaks = 20)


# Question 4. 
set.seed(3433)
data(AlzeheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
IL_col_idx
names(training[ ,IL_col_idx])
preProc <- preProcess(training[ ,IL_col_idx], 
                      method=c("center", "scale", "pca"), thresh = 0.9)
names(preProc)


# Question 5.
# We want to create training data set consisting of only the predictors 
# with variable names beginning with IL and the diagnosis. 
new_training <- training[,c(names(training[,IL_col_idx]), "diagnosis")]
names(new_training)
IL_col_idx <- grep("^[Ii][Ll].*", names(testing))
new_testing <- testing[,c(names(testing[,IL_col_idx]), "diagnosis")]
names(new_testing)
## building predictive models, using the predictors as they are
train_no_PCA <- train(diagnosis ~., data = new_training, method = "glm")
confusionMatrix(new_testing[, 13], predict(train_no_PCA, new_testing[, -13]))

## with PCA 
preProc <- preProcess(new_training[ ,-13], 
                      method=c("center", "scale", "pca"), thresh = 0.8)
trainPC <- predict(preProc,new_training[,-13])
modelFit <- train(new_training$diagnosis ~ .,method="glm",data=trainPC)
testPC <- predict(preProc,new_testing[,-13])
confusionMatrix(new_testing[,13], predict(modelFit,testPC))
