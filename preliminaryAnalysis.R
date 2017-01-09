source('helper_functions.R')
library(randomForest)
library(e1071)
library(caret)


data = fetchAndCleanData()
head(data)


#Create data for training
sample.ind <- sample(2, 
                     nrow(data),
                     replace = T,
                     prob = c(0.6,0.4))
data.dev <- data[sample.ind==1,]
data.val <- data[sample.ind==2,]

#See how data sets look as edible vs poisonous
table(data$Edible)/nrow(data)
table(data.dev$Edible)/nrow(data.dev)
table(data.val$Edible)/nrow(data.val)


rf = randomForest(Edible ~ CapShape + CapSurface + CapColor + Odor, 
                   ntree = 500,
                   data = data.dev)

plot(rf)
#Looks like after 100 trees there's not a huge reduction in error rate, 


varImpPlot(rf,
           sort = T,
           main="Variable Importance",
           n.var=4)
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
