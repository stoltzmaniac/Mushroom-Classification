
fetchAndCleanData = function(){
  
  theUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
  mushrooms <- read.table(file = theUrl, header = FALSE, sep = ",")
  
  
  
  #create a data frame with only the required columns
  #two data frames are created since there is a column break
  shrooms1 <- mushrooms[,1:4]
  shrooms2 <- mushrooms[,6]
  #the two are combined into one data frame
  shrooms <- cbind(shrooms1, shrooms2)
  
  
  #column names are added
  colnames(shrooms) <- c("Edible/Poisonous","Cap-Shape","Cap-Surface","Cap-Color","Odor")
  
  
  #Edible/Poisonous
  #Need to add the levels for every variable
  levels(shrooms$`Edible/Poisonous`) <- c(levels(shrooms$`Edible/Poisonous`), c("Edible","Poisonous"))
  shrooms$Edible/Poisonous <-
    shrooms$`Edible/Poisonous`[shrooms$`Edible/Poisonous` == "e"] <- "Edible"
  shrooms$`Edible/Poisonous`[shrooms$`Edible/Poisonous` == "p"] <- "Poisonous"
  
  #Cap-Shape
  levels(shrooms$`Cap-Shape`) <- c(levels(shrooms$`Cap-Shape`), c("Bell","Conical","Convex","Flat","Knobbed","Sunken"))
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "b"] <- "Bell"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "c"] <- "Conical"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "x"] <- "Convex"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "f"] <- "Flat"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "k"] <- "Knobbed"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "s"] <- "Sunken"
  
  #Cap-Surface
  levels(shrooms$`Cap-Surface`) <- c(levels(shrooms$`Cap-Surface`), c("Fibrous", "Grooves", "Scaly", "Smooth"))
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "f"] <- "Fibrous"
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "g"] <- "Grooves"
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "y"] <- "Scaly"
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "s"] <- "Smooth"
  
  #Cap-Color
  levels(shrooms$`Cap-Color`) <- c(levels(shrooms$`Cap-Color`), c("Brown", "Buff", "Cinnamon", "Gray", "Green", "Pink", "Purple", "Red", "White", "Yellow"))
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "n"] <- "Brown"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "b"] <- "Buff"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "c"] <- "Cinnamon"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "g"] <- "Gray"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "r"] <- "Green"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "p"] <- "Pink"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "u"] <- "Purple"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "e"] <- "Red"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "w"] <- "White"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "y"] <- "Yellow"
  
  #Odor
  levels(shrooms$Odor) <- c(levels(shrooms$Odor), c("Almond", "Anise", "Creosote", "Fishy", "Foul", "Musty", "None", "Pungent", "Spicy"))
  shrooms$Odor[shrooms$Odor == "a"] <- "Almond"
  shrooms$Odor[shrooms$Odor == "l"] <- "Anise"
  shrooms$Odor[shrooms$Odor == "c"] <- "Creosote"
  shrooms$Odor[shrooms$Odor == "y"] <- "Fishy"
  shrooms$Odor[shrooms$Odor == "f"] <- "Foul"
  shrooms$Odor[shrooms$Odor == "m"] <- "Musty"
  shrooms$Odor[shrooms$Odor == "n"] <- "None"
  shrooms$Odor[shrooms$Odor == "p"] <- "Pungent"
  shrooms$Odor[shrooms$Odor == "s"] <- "Spicy"
  
}
