source('helper_functions.R')
library(randomForest)
library(e1071)
library(caret)
library(ggplot2)

set.seed(123) 

data = fetchAndCleanData()
data.backup = data

head(data)

summary(data) #no missing data appears


#Create data for training
sample.ind <- sample(2, 
                     nrow(data),
                     replace = T,
                     prob = c(0.05,0.95))
data.dev <- data[sample.ind==1,]
data.val <- data[sample.ind==2,]

#See how data sets look as edible vs poisonous
table(data$Edible)/nrow(data)
table(data.dev$Edible)/nrow(data.dev)
table(data.val$Edible)/nrow(data.val)


varNames <- names(data.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("Edible")]

# Add response variable and convert to a formula object
rf.formula <- as.formula(paste("Edible", 
                       paste(varNames, 
                             collapse = "+"), 
                       sep = " ~ "))

rf = randomForest(rf.formula, 
                   ntree = 500,
                   data = data.dev)
plot(rf)
print(rf)

varImpPlot(rf,
           sort = T,
           main="Variable Importance")
#Looks like Odor is the greatest indicator


var.imp <- data.frame(importance(rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

#Odor and SporePrintColor are the best predictors
p = ggplot(data,aes(x=SporePrintColor, y=Odor, color = Edible))
p + geom_jitter(alpha=.25)

#Odor is too good of a predictor, let's drop it for fun
#So is SporePrintColor

data$Odor = NULL
data$SporePrintColor = NULL

#Create data for training
sample.ind <- sample(2, 
                     nrow(data),
                     replace = T,
                     prob = c(0.05,0.95))
data.dev <- data[sample.ind==1,]
data.val <- data[sample.ind==2,]

#See how data sets look as edible vs poisonous
table(data$Edible)/nrow(data)
table(data.dev$Edible)/nrow(data.dev)
table(data.val$Edible)/nrow(data.val)


varNames <- names(data.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("Edible")]

# Add response variable and convert to a formula object
rf.formula <- as.formula(paste("Edible", 
                               paste(varNames, 
                                     collapse = "+"), 
                               sep = " ~ "))

rf = randomForest(rf.formula, 
                  ntree = 500,
                  data = data.dev)
plot(rf)
print(rf)

varImpPlot(rf,
           sort = T,
           main="Variable Importance")
#Looks like Odor is the greatest indicator


var.imp <- data.frame(importance(rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

# Predicting response variable
data.dev$predicted.response <- predict(rf , data.dev)


## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data = data.dev$predicted.response,
                reference = data.dev$Edible,
                positive = 'Edible')


# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
confusionMatrix(data=data.val$predicted.response,
                reference=data.val$Edible,
                positive='Edible')


