```language-r
source('helper_functions.R')
library(randomForest)
library(e1071)
library(caret)
library(ggplot2)
set.seed(123) 
```
```language-r
#Import Data via Custom Function
data = fetchAndCleanData()
head(data)
```

```language-r
summary(data) #no missing data appears
```

```language-r
p = ggplot(data,aes(x=StalkColorBelowRing, 
                    y=StalkColorAboveRing,
                    color=Edible))

p + geom_jitter(alpha=0.3) + 
  scale_color_manual(breaks = c('Edible','Poisonous'),
                     values=c('darkgreen','red'))
```

```language-r
p = ggplot(data,aes(x=Odor, 
                    y=SporePrintColor, 
                    color=Edible))

p + geom_jitter(alpha=0.3) + 
  scale_color_manual(breaks = c('Edible','Poisonous'),
                     values=c('darkgreen','red'))
```

```language-r
p = ggplot(data,aes(x=Edible, 
                    y=Odor, 
                    color = Edible))

p + geom_jitter(alpha=0.2) + 
  scale_color_manual(breaks = c('Edible','Poisonous'),
                     values=c('darkgreen','red'))
```

```language-r
p = ggplot(data,aes(x=Edible, 
                    y=SporePrintColor, 
                    color = Edible))

p + geom_jitter(alpha=0.2) + 
  scale_color_manual(breaks = c('Edible','Poisonous'),
                     values=c('darkgreen','red'))
```


```language-r
#Create data for training
sample.ind = sample(2, 
                     nrow(data),
                     replace = T,
                     prob = c(0.05,0.95))
data.dev = data[sample.ind==1,]
data.val = data[sample.ind==2,]
```


#See how data sets look as edible vs poisonous
```language-r
# Original Data
table(data$Edible)/nrow(data)
```

```language-r
# Training Data
table(data.dev$Edible)/nrow(data.dev)

```

```language-r
# Testing Data
table(data.val$Edible)/nrow(data.val)
```



```


```language-r
#Fit Random Forest Model
rf = randomForest(Edible ~ ., 
                   ntree = 100,
                   data = data.dev)
plot(rf)
```


```language-r
print(rf)
```


```language-r
# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
```


```language-r
#Variable Importance
var.imp = data.frame(importance(rf,
                                 type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

```



```language-r
# Predicting response variable
data.dev$predicted.response = predict(rf , data.dev)

# Create Confusion Matrix
print(
confusionMatrix(data = data.dev$predicted.response,
                reference = data.dev$Edible,
                positive = 'Edible'))
```


```language-r
# Predicting response variable
data.val$predicted.response <- predict(rf ,data.val)

# Create Confusion Matrix
print(
confusionMatrix(data=data.val$predicted.response,
                reference=data.val$Edible,
                positive='Edible'))
```


