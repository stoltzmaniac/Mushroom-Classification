fetchAndCleanData = function(){
  
  # All of this code is from
  # https://rstudio-pubs-static.s3.amazonaws.com/125760_358e4a6802c94fa29e2a9ab49f45df94.html
  # I only switched it into a function to make it cleaner in my analysis
  
  theUrl = "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
  mushrooms = read.table(file = theUrl, header = FALSE, sep = ",")
  
  
  
  #create a data frame with only the required columns
  #two data frames are created since there is a column break
  shrooms1 = mushrooms[,1:4]
  shrooms2 = mushrooms[,6]
  #the two are combined into one data frame
  shrooms = cbind(shrooms1, shrooms2)
  
  
  #column names are added
  colnames(shrooms) = c("Edible","Cap-Shape","Cap-Surface","Cap-Color","Odor")
  
  
  #Edible
  #Need to add the levels for every variable
  levels(shrooms$`Edible`) = c(levels(shrooms$`Edible`), c("Edible","Poisonous"))
  shrooms$Edible = shrooms$`Edible`[shrooms$`Edible` == "e"] = "Edible"
  shrooms$`Edible`[shrooms$`Edible` == "p"] = "Poisonous"
  
  #Cap-Shape
  levels(shrooms$`Cap-Shape`) = c(levels(shrooms$`Cap-Shape`), c("Bell","Conical","Convex","Flat","Knobbed","Sunken"))
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "b"] = "Bell"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "c"] = "Conical"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "x"] = "Convex"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "f"] = "Flat"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "k"] = "Knobbed"
  shrooms$`Cap-Shape`[shrooms$`Cap-Shape` == "s"] = "Sunken"
  
  #Cap-Surface
  levels(shrooms$`Cap-Surface`) = c(levels(shrooms$`Cap-Surface`), c("Fibrous", "Grooves", "Scaly", "Smooth"))
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "f"] = "Fibrous"
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "g"] = "Grooves"
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "y"] = "Scaly"
  shrooms$`Cap-Surface`[shrooms$`Cap-Surface` == "s"] = "Smooth"
  
  #Cap-Color
  levels(shrooms$`Cap-Color`) = c(levels(shrooms$`Cap-Color`), c("Brown", "Buff", "Cinnamon", "Gray", "Green", "Pink", "Purple", "Red", "White", "Yellow"))
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "n"] = "Brown"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "b"] = "Buff"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "c"] = "Cinnamon"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "g"] = "Gray"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "r"] = "Green"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "p"] = "Pink"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "u"] = "Purple"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "e"] = "Red"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "w"] = "White"
  shrooms$`Cap-Color`[shrooms$`Cap-Color` == "y"] = "Yellow"
  
  #Odor
  levels(shrooms$Odor) = c(levels(shrooms$Odor), c("Almond", "Anise", "Creosote", "Fishy", "Foul", "Musty", "None", "Pungent", "Spicy"))
  shrooms$Odor[shrooms$Odor == "a"] = "Almond"
  shrooms$Odor[shrooms$Odor == "l"] = "Anise"
  shrooms$Odor[shrooms$Odor == "c"] = "Creosote"
  shrooms$Odor[shrooms$Odor == "y"] = "Fishy"
  shrooms$Odor[shrooms$Odor == "f"] = "Foul"
  shrooms$Odor[shrooms$Odor == "m"] = "Musty"
  shrooms$Odor[shrooms$Odor == "n"] = "None"
  shrooms$Odor[shrooms$Odor == "p"] = "Pungent"
  shrooms$Odor[shrooms$Odor == "s"] = "Spicy"
  
  return(shrooms)
}
