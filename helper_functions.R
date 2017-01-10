fetchAndCleanData = function(){
  
  # All of this code is from
  # https://rstudio-pubs-static.s3.amazonaws.com/125760_358e4a6802c94fa29e2a9ab49f45df94.html
  # I only switched it into a function to make it cleaner in my analysis
  
  theUrl = "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
  mushrooms = read.table(file = theUrl, header = FALSE, sep = ",")
  
  
  
  #create a data frame with only the required columns
  #two data frames are created since there is a column break
  #shrooms1 = mushrooms[,1:4]
  #shrooms2 = mushrooms[,6]
  #the two are combined into one data frame
  #shrooms = cbind(shrooms1, shrooms2)
  shrooms = mushrooms
  
  #column names are added
  colnames(shrooms) = c("Edible",
                        "CapShape",
                        "CapSurface",
                        "CapColor",
                        "Bruises",                        
                        "Odor",
                        "GillAttachment",
                        "GillSpacing",
                        "GillSize",
                        "GillColor",
                        "StalkShape",
                        "StalkRoot",
                        "StalkSurfaceAboveRing",
                        "StalkSurfaceBelowRing",
                        "StalkColorAboveRing",
                        "StalkColorBelowRing",
                        "VeilType",
                        "VeilColor",
                        "RingNumber",
                        "RingType",
                        "SporePrintColor",
                        "Population",
                        "Habitat")

  
  #Edible
  shrooms$Edible = as.character(shrooms$Edible)
  shrooms$Edible[shrooms$Edible == "e"] = "Edible"
  shrooms$Edible[shrooms$Edible == 'p'] = "Poisonous"
  shrooms$Edible = factor(shrooms$Edible)
  
  
  # Edible
  #levels(shrooms$Edible) = c(levels(shrooms$Edible), c("Poisonous","Edible"))
  #shrooms$Edible[shrooms$Edible == "p"] = "Poisonous"
  #shrooms$Edible[shrooms$Edible == "e"] = "Edible"
  
  #CapShape
  levels(shrooms$`CapShape`) = c(levels(shrooms$`CapShape`), c("Bell","Conical","Convex","Flat","Knobbed","Sunken"))
  shrooms$`CapShape`[shrooms$`CapShape` == "b"] = "Bell"
  shrooms$`CapShape`[shrooms$`CapShape` == "c"] = "Conical"
  shrooms$`CapShape`[shrooms$`CapShape` == "x"] = "Convex"
  shrooms$`CapShape`[shrooms$`CapShape` == "f"] = "Flat"
  shrooms$`CapShape`[shrooms$`CapShape` == "k"] = "Knobbed"
  shrooms$`CapShape`[shrooms$`CapShape` == "s"] = "Sunken"
  
  #CapSurface
  levels(shrooms$`CapSurface`) = c(levels(shrooms$`CapSurface`), c("Fibrous", "Grooves", "Scaly", "Smooth"))
  shrooms$`CapSurface`[shrooms$`CapSurface` == "f"] = "Fibrous"
  shrooms$`CapSurface`[shrooms$`CapSurface` == "g"] = "Grooves"
  shrooms$`CapSurface`[shrooms$`CapSurface` == "y"] = "Scaly"
  shrooms$`CapSurface`[shrooms$`CapSurface` == "s"] = "Smooth"
  
  #CapColor
  levels(shrooms$`CapColor`) = c(levels(shrooms$`CapColor`), c("Brown", "Buff", "Cinnamon", "Gray", "Green", "Pink", "Purple", "Red", "White", "Yellow"))
  shrooms$`CapColor`[shrooms$`CapColor` == "n"] = "Brown"
  shrooms$`CapColor`[shrooms$`CapColor` == "b"] = "Buff"
  shrooms$`CapColor`[shrooms$`CapColor` == "c"] = "Cinnamon"
  shrooms$`CapColor`[shrooms$`CapColor` == "g"] = "Gray"
  shrooms$`CapColor`[shrooms$`CapColor` == "r"] = "Green"
  shrooms$`CapColor`[shrooms$`CapColor` == "p"] = "Pink"
  shrooms$`CapColor`[shrooms$`CapColor` == "u"] = "Purple"
  shrooms$`CapColor`[shrooms$`CapColor` == "e"] = "Red"
  shrooms$`CapColor`[shrooms$`CapColor` == "w"] = "White"
  shrooms$`CapColor`[shrooms$`CapColor` == "y"] = "Yellow"

  # Bruises
  levels(shrooms$Bruises) = c(levels(shrooms$Bruises), c("True","False"))
  shrooms$Bruises[shrooms$Bruises == "t"] = "True"
  shrooms$Bruises[shrooms$Bruises == "f"] = "False"
    
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
  
  # GillAttachment
  levels(shrooms$GillAttachment) = c(levels(shrooms$GillAttachment), c("Attached","Descending","Free","Notched"))
  shrooms$GillAttachment[shrooms$GillAttachment == "a"] = "Attached"
  shrooms$GillAttachment[shrooms$GillAttachment == "d"] = "Descending"
  shrooms$GillAttachment[shrooms$GillAttachment == "f"] = "Free"
  shrooms$GillAttachment[shrooms$GillAttachment == "n"] = "Notched"
  
  # GillSpacing
  levels(shrooms$GillSpacing) = c(levels(shrooms$GillSpacing), c("Close","Crowded","Distant"))
  shrooms$GillSpacing[shrooms$GillSpacing == "c"] = "Close"
  shrooms$GillSpacing[shrooms$GillSpacing == "w"] = "Crowded"
  shrooms$GillSpacing[shrooms$GillSpacing == "d"] = "Distant"
  
  # GillSize
  levels(shrooms$GillSize) = c(levels(shrooms$GillSize), c("Broad","Narrow"))
  shrooms$GillSize[shrooms$GillSize == "b"] = "Broad"
  shrooms$GillSize[shrooms$GillSize == "n"] = "Narrow"
  
  # GillColor
  levels(shrooms$GillColor) = c(levels(shrooms$GillColor), c("Black","Brown","Buff","Chocolate","Gray","Green","Orange","Pink","Purple","Red","White","Yellow"))
  shrooms$GillColor[shrooms$GillColor == "k"] = "Black"
  shrooms$GillColor[shrooms$GillColor == "n"] = "Brown"
  shrooms$GillColor[shrooms$GillColor == "b"] = "Buff"
  shrooms$GillColor[shrooms$GillColor == "h"] = "Chocolate"
  shrooms$GillColor[shrooms$GillColor == "g"] = "Gray"
  shrooms$GillColor[shrooms$GillColor == "r"] = "Green"
  shrooms$GillColor[shrooms$GillColor == "o"] = "Orange"
  shrooms$GillColor[shrooms$GillColor == "p"] = "Pink"
  shrooms$GillColor[shrooms$GillColor == "u"] = "Purple"
  shrooms$GillColor[shrooms$GillColor == "e"] = "Red"
  shrooms$GillColor[shrooms$GillColor == "w"] = "White"
  shrooms$GillColor[shrooms$GillColor == "y"] = "Yellow"
  
  # StalkShape
  levels(shrooms$StalkShape) = c(levels(shrooms$StalkShape), c("Enlarging","Tapering"))
  shrooms$StalkShape[shrooms$StalkShape == "e"] = "Enlarging"
  shrooms$StalkShape[shrooms$StalkShape == "t"] = "Tapering"
  
  # StalkRoot
  levels(shrooms$StalkRoot) = c(levels(shrooms$StalkRoot), c("Bulbous","Club","Cup","Equal","Rhizomorphs","Rooted","Missing"))
  shrooms$StalkRoot[shrooms$StalkRoot == "b"] = "Bulbous"
  shrooms$StalkRoot[shrooms$StalkRoot == "c"] = "Club"
  shrooms$StalkRoot[shrooms$StalkRoot == "u"] = "Cup"
  shrooms$StalkRoot[shrooms$StalkRoot == "e"] = "Equal"
  shrooms$StalkRoot[shrooms$StalkRoot == "z"] = "Rhizomorphs"
  shrooms$StalkRoot[shrooms$StalkRoot == "r"] = "Rooted"
  shrooms$StalkRoot[shrooms$StalkRoot == "?"] = "Missing"
  
  # StalkSurfaceAboveRing
  levels(shrooms$StalkSurfaceAboveRing) = c(levels(shrooms$StalkSurfaceAboveRing), c("Fibrous","Scaly","Silky","Smooth"))
  shrooms$StalkSurfaceAboveRing[shrooms$StalkSurfaceAboveRing == "f"] = "Fibrous"
  shrooms$StalkSurfaceAboveRing[shrooms$StalkSurfaceAboveRing == "y"] = "Scaly"
  shrooms$StalkSurfaceAboveRing[shrooms$StalkSurfaceAboveRing == "k"] = "Silky"
  shrooms$StalkSurfaceAboveRing[shrooms$StalkSurfaceAboveRing == "s"] = "Smooth"

  # StalkSurfaceBelowRing
  levels(shrooms$StalkSurfaceBelowRing) = c(levels(shrooms$StalkSurfaceBelowRing), c("Fibrous","Scaly","Silky","Smooth"))
  shrooms$StalkSurfaceBelowRing[shrooms$StalkSurfaceBelowRing == "f"] = "Fibrous"
  shrooms$StalkSurfaceBelowRing[shrooms$StalkSurfaceBelowRing == "y"] = "Scaly"
  shrooms$StalkSurfaceBelowRing[shrooms$StalkSurfaceBelowRing == "k"] = "Silky"
  shrooms$StalkSurfaceBelowRing[shrooms$StalkSurfaceBelowRing == "s"] = "Smooth"
  
  # StalkColorAboveRing
  levels(shrooms$StalkColorAboveRing) = c(levels(shrooms$StalkColorAboveRing), c("Brown","Buff","Cinnamon","Gray","Orange","Pink","Red","White","Yellow"))
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "n"] = "Brown"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "b"] = "Buff"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "c"] = "Cinnamon"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "g"] = "Gray"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "o"] = "Orange"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "p"] = "Pink"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "e"] = "Red"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "w"] = "White"
  shrooms$StalkColorAboveRing[shrooms$StalkColorAboveRing == "y"] = "Yellow"
  
  
  # StalkColorBelowRing
  levels(shrooms$StalkColorBelowRing) = c(levels(shrooms$StalkColorBelowRing), c("Brown","Buff","Cinnamon","Gray","Orange","Pink","Red","White","Yellow"))
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "n"] = "Brown"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "b"] = "Buff"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "c"] = "Cinnamon"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "g"] = "Gray"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "o"] = "Orange"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "p"] = "Pink"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "e"] = "Red"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "w"] = "White"
  shrooms$StalkColorBelowRing[shrooms$StalkColorBelowRing == "y"] = "Yellow"
  
  # VeilType
  levels(shrooms$VeilType) = c(levels(shrooms$VeilType), c("Partial","Universal"))
  shrooms$VeilType[shrooms$VeilType == "p"] = "Partial"
  shrooms$VeilType[shrooms$VeilType == "u"] = "Universal"
  
  # VeilColor
  levels(shrooms$VeilColor) = c(levels(shrooms$VeilColor), c("Brown","Orange","White","Yellow"))
  shrooms$VeilColor[shrooms$VeilColor == "n"] = "Brown"
  shrooms$VeilColor[shrooms$VeilColor == "o"] = "Orange"
  shrooms$VeilColor[shrooms$VeilColor == "w"] = "White"
  shrooms$VeilColor[shrooms$VeilColor == "y"] = "Yellow"
  
  # RingNumber
  levels(shrooms$RingNumber) = c(levels(shrooms$RingNumber), c("None","One","Two"))
  shrooms$RingNumber[shrooms$RingNumber == "n"] = "None"
  shrooms$RingNumber[shrooms$RingNumber == "o"] = "One"
  shrooms$RingNumber[shrooms$RingNumber == "t"] = "Two"
  
  # RingType
  levels(shrooms$RingType) = c(levels(shrooms$RingType), c("Cobwebby","Evanescent","Flaring","Large","None","Pendant","Sheathing","Zone"))
  shrooms$RingType[shrooms$RingType == "c"] = "Cobwebby"
  shrooms$RingType[shrooms$RingType == "e"] = "Evanescent"
  shrooms$RingType[shrooms$RingType == "f"] = "Flaring"
  shrooms$RingType[shrooms$RingType == "l"] = "Large"
  shrooms$RingType[shrooms$RingType == "n"] = "None"
  shrooms$RingType[shrooms$RingType == "p"] = "Pendant"
  shrooms$RingType[shrooms$RingType == "s"] = "Sheathing"
  shrooms$RingType[shrooms$RingType == "z"] = "Zone"
  
  # SporePrintColor
  levels(shrooms$SporePrintColor) = c(levels(shrooms$SporePrintColor), c("Black","Brown","Buff","Chocolate","Green","Orange","Purple","White","Yellow"))
  shrooms$SporePrintColor[shrooms$SporePrintColor == "k"] = "Black"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "n"] = "Brown"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "b"] = "Buff"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "h"] = "Chocolate"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "r"] = "Green"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "o"] = "Orange"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "u"] = "Purple"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "w"] = "White"
  shrooms$SporePrintColor[shrooms$SporePrintColor == "y"] = "Yellow"
  
  # Population
  levels(shrooms$Population) = c(levels(shrooms$Population), c("Abundnant","Clustered","Numerous","Scattered","Several","Solitary"))
  shrooms$Population[shrooms$Population == "a"] = "Abundnant"
  shrooms$Population[shrooms$Population == "c"] = "Clustered"
  shrooms$Population[shrooms$Population == "n"] = "Numerous"
  shrooms$Population[shrooms$Population == "s"] = "Scattered"
  shrooms$Population[shrooms$Population == "v"] = "Several"
  shrooms$Population[shrooms$Population == "y"] = "Solitary"
  
  # Habitat
  levels(shrooms$Habitat) = c(levels(shrooms$Habitat), c("Grasses","Leaves","Meadows","Paths","Urban","Waste","Woods"))
  shrooms$Habitat[shrooms$Habitat == "g"] = "Grasses"
  shrooms$Habitat[shrooms$Habitat == "l"] = "Leaves"
  shrooms$Habitat[shrooms$Habitat == "m"] = "Meadows"
  shrooms$Habitat[shrooms$Habitat == "p"] = "Paths"
  shrooms$Habitat[shrooms$Habitat == "u"] = "Urban"
  shrooms$Habitat[shrooms$Habitat == "w"] = "Waste"
  shrooms$Habitat[shrooms$Habitat == "d"] = "Woods"

  return(shrooms)
}
