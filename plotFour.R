library(ggplot2)

plotFour = function(){
  
  amtPerDept = amtPerDept()
  
  amtPerType = amtPerType()
  typeNames = as.factor(as.character(lapply(1:length(amtPerType), function(x) amtPerType[[x]][[3]])))
  typeValues = as.numeric(as.character(lapply(1:length(amtPerType), function(x) amtPerType[[x]][[4]])))
  levels(typeNames) = typeNames
  
  amtPerVend = amtPerVend()  
  vendNames = as.factor(as.character(lapply(1:length(amtPerVend), function(x) amtPerVend[[x]][[3]])))
  vendValues = as.numeric(as.character(lapply(1:length(amtPerVend), function(x) amtPerVend[[x]][[4]])))
  levels(vendNames) = vendNames
  
  amtPerTypeVend = amtPerTypeVend()  
  typeVendNames = as.factor(as.character(lapply(1:length(amtPerTypeVend), function(x) amtPerTypeVend[[x]][[3]])))
  typeVendValues = as.numeric(as.character(lapply(1:length(amtPerTypeVend), function(x) amtPerTypeVend[[x]][[4]])))
  levels(typeVendNames) = typeVendNames

  
  purchases2 = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  cont = 'y'
  i = 1
  while(cont == 'y'){
    clearPlot()
    par(mfrow = c(2,2))
    
    plot(as.factor(amtPerDept[,1]), amtPerDept[,2], main = "Amount per Department")
    text(amtPerDept[,1],amtPerDept[,2]+50, round(amtPerDept[,2]), cex = 0.8)
    points(x = purchases2[i, department], y = purchases2[i, 'TRAN AMT'], col = 'red')
    
    
    plot(typeNames, typeValues, main = "Amount per Cost Code")
    text(typeNames, typeValues+500, round(typeValues), cex=0.8)
    section = findSection(amtPerType, purchases2[i, type])
    points(x = match(section, levels(typeNames)), y = purchases2[i, 'TRAN AMT'], col = 'red')
    
    
    plot(vendNames, vendValues, main = "Amount per SIC Code")
    text(vendNames, vendValues+500, round(vendValues), cex=0.8)
    section = findSection(amtPerVend, purchases2[i, vendor])
    points(x = match(section, levels(vendNames)), y = purchases2[i, 'TRAN AMT'], col = 'red')
    
    
    plot(typeVendNames, typeVendValues, main = "Amount per Cost Code and SIC Code")
    text(typeVendNames, typeVendValues+300, round(typeVendValues), cex=0.8)
    section = findSection(amtPerTypeVend, as.numeric(paste(purchases2[i, type], purchases2[i, vendor], sep = '.')))
    points(x = section, y = purchases2[i, 'TRAN AMT'], col = 'red')
    
    
    cont = readline("Plot next? (y/n) \n")
    i = i + 1
  }
}

findSection = function(data, value){
  
  for(i in 1:length(data)){
    if(value %in% data[[i]][[1]])
      return(data[[i]][[3]])
  }
  
}
