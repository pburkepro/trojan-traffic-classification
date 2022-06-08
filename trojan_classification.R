# Import necessary libraries
library(C50)
library(rpart)
library(gmodels)
library(caret)
library(neuralnet)
library(Amelia)
library(pscl)
library(ROCR)

set.seed(123)
Trojan = read.csv("Trojan_Detection.csv")

# Rename Class column to Traffic.Type
names(Trojan)[names(Trojan) == "Class"] = "Traffic.Type"

# Drop X & Flow.ID
Trojan = Trojan[-c(1, 2)]

#Convert Source.IP & Port, Destination.IP & Port, Timestamp to numeric factors
Trojan$Source.IP = as.numeric(factor(Trojan$Source.IP))
Trojan$Source.Port = as.numeric(factor(Trojan$Source.Port))
Trojan$Destination.IP = as.numeric(factor(Trojan$Destination.IP))
Trojan$Destination.Port = as.numeric(factor(Trojan$Destination.Port))
Trojan$Timestamp = as.numeric(factor(Trojan$Timestamp))

# Create sample data frame from larger set
Trojan.Sample = Trojan[sample(nrow(Trojan), 800), ]
Trojan.Sample$Traffic.Type = factor(Trojan.Sample$Traffic.Type,
                                    levels = c("Benign", "Trojan"))
str(Trojan.Sample)

# Normalize & clean Trojan.Sample data set
Normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

# Test Normalize function
Normalize(c(1, 2, 3, 4, 5))
Normalize(c(10, 20, 30, 40, 50))
Normalize(c(0,0,0,1,1))

Trojan.Normalized = as.data.frame(lapply(Trojan.Sample[1:83], Normalize)) 
Trojan.Cleaned = Trojan.Normalized[ , colSums(is.na(Trojan.Normalized)) == 0]
Trojan.Cleaned[72] <- Trojan.Sample[84]

# Test Normalize function on Trojan column 
summary(Trojan.Cleaned$Flow.Duration)

# Check ratio of outcome variable
table(Trojan.Cleaned$Traffic.Type)
round(prop.table(table(Trojan.Cleaned$Traffic.Type)) * 100, digits = 1)

# Look at two characteristics of the Trojan.Cleaned that might be predictors of Traffic.Type
summary(Trojan.Cleaned$Destination.IP)
summary(Trojan.Cleaned$Destination.Port)

set.seed(123)
Partition = sample(2, nrow(Trojan.Cleaned), replace=TRUE, prob=c(0.7, 0.3))

# Randomly sample training and testing data
Trojan.Cleaned.Train = Trojan.Cleaned[Partition==1,]
Trojan.Cleaned.Test = Trojan.Cleaned[Partition==2,]

# Return target variables
prop.table(table(Trojan.Cleaned.Train$Traffic.Type))
prop.table(table(Trojan.Cleaned.Test$Traffic.Type))

# We want to remove the Traffic.Type class from training but use it as our target variable
Trojan.Cleaned.Model <- C5.0(Trojan.Cleaned.Train[-72], as.factor(Trojan.Cleaned.Train$Traffic.Type))

# Display simple facts about the tree
Trojan.Cleaned.Model

# Display detailed information about the tree
summary(Trojan.Cleaned.Model)

# create a factor vector of predictions on test data
Trojan.Cleaned.Pred <- predict(Trojan.Cleaned.Model, Trojan.Cleaned.Test)

# cross tabulation of predicted versus actual classes
CrossTable(Trojan.Cleaned.Test$Traffic.Type, Trojan.Cleaned.Pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Traffic.Type', 'Predicted Traffic.Type'))

# Boosted decision tree with 10 trials
Trojan.Cleaned.Boost10 <- C5.0(Trojan.Cleaned.Train[-72], as.factor(Trojan.Cleaned.Train$Traffic.Type),
                               trials = 10)

Trojan.Cleaned.Boost10
summary(Trojan.Cleaned.Boost10)

Trojan.Cleaned.Boost.Pred10 <- predict(Trojan.Cleaned.Boost10, Trojan.Cleaned.Test)

CrossTable(Trojan.Cleaned.Test$Traffic.Type, Trojan.Cleaned.Boost.Pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Traffic.Type', 'predicted Traffic.Type'))

# Boosted with 9 iterations and rpart() hyper-parameter control
rpart.control(minsplit = 50, cp = 0.01)
Trojan.Cleaned.Boost9 <- C5.0(Trojan.Cleaned.Train[-72], as.factor(Trojan.Cleaned.Train$Traffic.Type),
                              trials = 9)

Trojan.Cleaned.Boost9
summary(Trojan.Cleaned.Boost9)

Trojan.Cleaned.Boost.Pred9 <- predict(Trojan.Cleaned.Boost9, Trojan.Cleaned.Test)

CrossTable(Trojan.Cleaned.Test$Traffic.Type, Trojan.Cleaned.Boost.Pred9,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Traffic.Type', 'predicted Traffic.Type'))

# Create dimensions for a cost matrix
Matrix.Dimensions <- list(c("Benign", "Trojan"), c("Benign", "Trojan"))
names(Matrix.Dimensions) <- c("Predicted", "Actual")
Matrix.Dimensions

# Build the matrix

# If we say Trojan traffic costs the company 5 times as much as a false positive Benign traffic 
# our penalty values will appear as such:
Error.Cost <- matrix(c(0, 1, 5, 0), nrow = 2, dimnames = Matrix.Dimensions)
Error.Cost

# We are saying there is no cost if the algorithm classifies correctly
# but a false negative costs 5 vs. a false positive costs 1. 

# Apply the cost matrix to the tree
Trojan.Cleaned.Cost <- C5.0(Trojan.Cleaned.Train[-72], as.factor(Trojan.Cleaned.Train$Traffic.Type),
                            costs = Error.Cost)

Trojan.Cleaned.Cost.Pred <- predict(Trojan.Cleaned.Cost, Trojan.Cleaned.Test)

CrossTable(Trojan.Cleaned.Test$Traffic.Type, Trojan.Cleaned.Cost.Pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Traffic.Type', 'Predicted Traffic.Type'))

confusionMatrix(Trojan.Cleaned.Cost.Pred, as.factor(Trojan.Cleaned.Test$Traffic.Type))

Trojan.Cleaned.Train$Traffic.Type[1:10]

# Set Traffic.Type to a numeric Factor (1 = Benign, 2 = Trojan)
Trojan.Cleaned.Train$Traffic.Type = as.numeric(factor(Trojan.Cleaned.Train$Traffic.Type))
Trojan.Cleaned.Test$Traffic.Type = as.numeric(factor(Trojan.Cleaned.Test$Traffic.Type))
Trojan.Cleaned.Train$Traffic.Type[1:10]

# simple ANN with only a single hidden neuron
set.seed(12345)
Trojan.Cleaned.ANN.Model <- neuralnet(formula = Traffic.Type ~., 
                                      data = Trojan.Cleaned.Train)
# Visualize the network topology
plot(Trojan.Cleaned.ANN.Model)

ANN.Model.Results <- compute(Trojan.Cleaned.ANN.Model, Trojan.Cleaned.Test[1:71])

# Obtain predicted Traffic Types
Predicted.Traffic.Type <- ANN.Model.Results$net.result

# Examine the correlation between predicted and actual values
cor(Predicted.Traffic.Type, as.numeric(Trojan.Cleaned.Test$Traffic.Type))

# A more complex neural network topology with 5 hidden neurons
set.seed(12345)
Trojan.Cleaned.ANN.Model.2 <- neuralnet(formula = Traffic.Type ~., 
                                        data = Trojan.Cleaned.Train, hidden = 5)
# Visualize the network topology
plot(Trojan.Cleaned.ANN.Model.2)


# Evaluate the results as before
ANN.Model.Results.2 <- compute(Trojan.Cleaned.ANN.Model.2, Trojan.Cleaned.Test[1:71])
Predicted.Traffic.Type.2 <- ANN.Model.Results.2$net.result
cor(Predicted.Traffic.Type.2, Trojan.Cleaned.Test$Traffic.Type)

set.seed(12345)
Trojan.Cleaned.ANN.Model.3 <- neuralnet(Traffic.Type ~ .,
                                        data = Trojan.Cleaned.Train, hidden = c(3,2))

# Plot the network
plot(Trojan.Cleaned.ANN.Model.3)

# Evaluate the results as before
ANN.Model.Results.3 <- compute(Trojan.Cleaned.ANN.Model.3, Trojan.Cleaned.Test[1:71])
Predicted.Traffic.Type.3 <- ANN.Model.Results.3$net.result
cor(Predicted.Traffic.Type.3, Trojan.Cleaned.Test$Traffic.Type)

Trojan.Cleaned.Train$Traffic.Type[Trojan.Cleaned.Train$Traffic.Type == 2] <- 0
Trojan.Cleaned.Train$Traffic.Type[1:10]

Trojan.Cleaned.Test$Traffic.Type[Trojan.Cleaned.Test$Traffic.Type == 2] <- 0
Trojan.Cleaned.Test$Traffic.Type[1:10]

# Fit the model. We must specify the parameter family=binomial in the glm() function.
Trojan.Cleaned.GLM <- glm(Traffic.Type ~., family=binomial(link='logit'), data=Trojan.Cleaned.Train)
summary(Trojan.Cleaned.GLM)

anova(Trojan.Cleaned.GLM, test="Chisq")
pR2(Trojan.Cleaned.GLM)

Fitted.Results <- predict(Trojan.Cleaned.GLM, newdata=Trojan.Cleaned.Test[-72], type='response')
Fitted.Results <-ifelse(Fitted.Results>0.5, 1, 0)
Fitted.Results <- as.factor(Fitted.Results)

#Predicting using logistic model
str(Fitted.Results)

# Determine those mis-classified 
misClassifiedError <- mean(Fitted.Results != Trojan.Cleaned.Test$Traffic.Type)
confusionMatrix(Fitted.Results, as.factor(Trojan.Cleaned.Test$Traffic.Type))

# Determine the accuracy of the model
print(paste('We find an accuracy of: ',1-misClassifiedError))

Predict <- predict(Trojan.Cleaned.GLM, newdata=Trojan.Cleaned.Test[-72], type="response")
Predictions <- prediction(Predict, Trojan.Cleaned.Test$Traffic.Type)
Performance <- performance(Predictions, measure = "tpr", x.measure = "fpr")
plot(Performance)

#Calculate the area under the curve (AUC)
auc <- performance(Predictions, measure = "auc")
auc <- auc@y.values[[1]]
auc
