# importing data

data <- read.csv("cardiotocographic.csv", T, ",")

# cheaking the structure of the data

str(data)

# spliting the data into train and test

library(caTools)

set.seed(234)

split <- sample.split(data, SplitRatio = .75)

train <- subset(data, split == T)
test <- subset(data, split == F)

# cheacking the corralation of the data

library(psych)

pairs.panels(train[,-22],
             gap= 0,
             pch= 21)

# training the data with pca

pca <- prcomp(train[,-22],
              scale. = T,
              center = T)

# cheacking which attributes are available with pca

attributes(pca)
pca$center

# analysing the pca model

print(pca)
summary(pca)

# creating a map of our pca model
# it will be messy to map because of so many dimension

library(ggfortify)

autoplot(prcomp(train), colour = "NSP", loadings= T,
         loadings.colour = "blue", loadings.label = T, loadings.label.size=3,
         frame= T, frame.colour= "NSP")

# predicting traing data with pca

pca.train <- predict(pca, train)
pca.train <- as.data.frame(pca.train)

# adding our target variable which was not included during the pca training

pca.train <- cbind(pca.train, NSP= train[,22])

# predicting test data with pca

pca.test <- predict(pca, test)
pca.test <- as.data.frame(pca.test)

# doing same for the test data

pca.test <- cbind(pca.test, NSP= test[,22])

#model with multinomial logistic regression

# changing NSP variable into factor variable for our model

pca.train$NSP <- as.factor(pca.train$NSP)
pca.test$NSP <- as.factor(pca.test$NSP)

library(nnet)

pca.train$NSP <- relevel(pca.train$NSP, ref = "1")

# we are selecting first 14 variable because together they are responsible for more than 95% of the variability of the data

model <- multinom(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14,
                  data = pca.train)

# running randomForest for the same data

library(randomForest)

model1 <- randomForest(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14,
                       data = pca.train)


# prediction with the multinomial logistic regression

library(caret)

pred.train <- predict(model, newdata = pca.train)
confusionMatrix(pred.train, pca.train$NSP)

pred.test <- predict(model, newdata = pca.test)
confusionMatrix(pred.test, pca.test$NSP)

# prediction with the randomForest 

pred.trainR <- predict(model1, newdata = pca.train)
confusionMatrix(pred.trainR, pca.train$NSP)

pred.testR <- predict(model1, newdata = pca.test)
confusionMatrix(pred.testR, pca.test$NSP)
