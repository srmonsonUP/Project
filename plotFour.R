library(ggplot2)

plotFour = function(){
  
  amtPerDept = amtPerDept()
  
  
  amtPerType = amtPerType()
  typeNames = as.factor(as.character(lapply(1:length(amtPerType), function(x) amtPerType[[x]][[3]])))
  typeValues = as.numeric(as.character(lapply(1:length(amtPerType), function(x) amtPerType[[x]][[4]])))
  typeDev = as.numeric(as.character(lapply(1:length(amtPerType), function(x) amtPerType[[x]][[5]])))
  typeData = cbind.data.frame(typeNames, typeValues, typeDev)
  typeData[,1] = factor(typeData[,1], typeData[,1])
  
    
  amtPerVend = amtPerVend()  
  vendNames = as.factor(as.character(lapply(1:length(amtPerVend), function(x) amtPerVend[[x]][[3]])))
  vendValues = as.numeric(as.character(lapply(1:length(amtPerVend), function(x) amtPerVend[[x]][[4]])))
  vendDev = as.numeric(as.character(lapply(1:length(amtPerVend), function(x) amtPerVend[[x]][[4]])))
  vendData = cbind.data.frame(vendNames, vendValues, vendDev)
  vendData[,1] = factor(vendData[,1], vendData[,1]) 
  
  amtPerTypeVend = amtPerTypeVend()  
  typeVendNames = as.factor(as.character(lapply(1:length(amtPerTypeVend), function(x) amtPerTypeVend[[x]][[3]])))
  typeVendValues = as.numeric(as.character(lapply(1:length(amtPerTypeVend), function(x) amtPerTypeVend[[x]][[4]])))
  typeVendDev = as.numeric(as.character(lapply(1:length(amtPerTypeVend), function(x) amtPerTypeVend[[x]][[5]])))
  typeVendData = cbind.data.frame(typeVendNames, typeVendValues, typeVendDev)
  typeVendData[,1] = factor(typeVendData[,1], typeVendData[,1])
  
  purchases2 = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  cont = ''
  i = 20
  while(cont == ''){
    clearPlot()
    par(mfrow = c(2,2), las = 2)
    
    plot(as.factor(amtPerDept[,1]), amtPerDept[,2], main = "Amount per Department")
    text(amtPerDept[,1],amtPerDept[,2]+50, round(amtPerDept[,2]), cex = 0.8)
    points(x = purchases2[i, department], y = purchases2[i, 'TRAN AMT'], col = 'red', cex = 1.5)
    sdev = (purchases2[i, 'TRAN AMT'] - amtPerDept[amtPerDept[,1] == purchases2[i, department],2]) / amtPerDept[amtPerDept[,1] ==purchases2[i, department],3]
    usr = par("usr")
    text(usr[1], usr[4], paste("SD =", round(sdev, 2)), col = 'red', cex = 1.5, adj = c(0,1))
    
    
    plot(typeData[,1], typeData[,2], main = "Amount per Cost Code")
    text(typeData[,1], typeData[,2]+500, round(typeData[,2]), cex=0.8)
    section = findSection(amtPerType, purchases2[i, type])
    points(x = typeData[typeData[,1] == section, 1], y = purchases2[i, 'TRAN AMT'], col = 'red', cex = 1.5)
    sdev = (purchases2[i, 'TRAN AMT'] - typeData[typeData[,1] == section, 2]) / typeData[typeData[,1] == section, 3]
    usr = par("usr")
    text(usr[1], usr[4], paste("SD =", round(sdev, 2)), col = 'red', cex = 1.5, adj = c(0,1))
    
    
    plot(vendData[,1], vendData[,2], main = "Amount per SIC Code")
    text(vendData[,1], vendData[,2]+500, round(vendData[,2]), cex=0.8)
    section = findSection(amtPerVend, purchases2[i, vendor])
    points(x = vendData[vendData[,1] == section, 1], y = purchases2[i, 'TRAN AMT'], col = 'red', cex = 1.5)
    sdev = (purchases2[i, 'TRAN AMT'] - vendData[vendData[,1] == section, 2]) / vendData[vendData[,1] == section, 3]
    usr = par("usr")
    text(usr[1], usr[4], paste("SD =", round(sdev, 2)), col = 'red', cex = 1.5, adj = c(0,1))
    
    
    plot(typeVendData[,1], typeVendData[,2], main = "Amount per Cost Code and SIC Code")
    text(typeVendData[,1], typeVendData[,2]+300, round(typeVendData[,2]), cex=0.8)
    section = findSection(amtPerTypeVend, as.numeric(paste(purchases2[i, type], purchases2[i, vendor], sep = '.')))
    points(x = typeVendData[typeVendData[,1] == section,1], y = purchases2[i, 'TRAN AMT'], col = 'red', cex = 1.5)
    sdev = (purchases2[i, 'TRAN AMT'] - typeVendData[typeVendData[,1] == section, 2]) / typeVendData[typeVendData[,1] == section, 3]
    usr = par("usr")
    text(usr[1], usr[4], paste("SD =", round(sdev, 2)), col = 'red', cex = 1.5, adj = c(0,1))
    
    
   
    i = i + 1
    cont = readline("Plot next? Any character to quit")
  }
}

findSection = function(data, value){
  for(i in 1:length(data)){
    if(value %in% as.numeric(as.character(data[[i]][[1]])))
      return(data[[i]][[3]])
  }
}

findMean = function(data, value){
  for(i in 1:length(data))
    if(value == data[[i]][[3]])
      return(as.numeric(data[[i]][[4]]))
}
