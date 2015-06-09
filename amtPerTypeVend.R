library("rgl")

amtPerTypeVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
 
  avg = tapply(purchases$'TRAN AMT', list(purchases[,type], purchases[,vendor]), mean, na.rm=T)
  sd = tapply(purchases$'TRAN AMT', list(purchases[,type], purchases[,vendor]), sd, na.rm=T)
  
  x = which(avg > 0, arr.ind = T)
  
  avgPerTypeVend = c()
  for(i in 1:nrow(x)){
    avgPerTypeVend = c(avgPerTypeVend, c(rownames(avg)[x[i,1]], colnames(avg)[x[i,2]], avg[x[i,1], x[i,2]]))
  }
  avgPerTypeVend = matrix(avgPerTypeVend, ncol = 3, byrow = T)
  
  avgPerTypeVend[,1] = paste(avgPerTypeVend[,1], ".", avgPerTypeVend[,2], sep = "")
  avgPerTypeVend[,2] = avgPerTypeVend[,3]
  avgPerTypeVend = avgPerTypeVend[,-3]

  categories = createCategories(as.data.frame(avgPerTypeVend), c(type, vendor))
 
  
  return(categories)
}








createCategories2 = function(data){
  
  typeCat = createCategories(as.data.frame(data[,c(1,3)]))
  vendCat = createCategories(as.data.frame(data[,c(2,3)]))
  
  while(nrow(data) > 0){
    typeCatName = valToName(data[1,1], typeCat)
    vendCatName = valToName(data[1,2], vendCat)
    
    cat = data[data[,1] %in% nameToVal(typeCatName, typeCat) & data[,2] %in% nameToVal(vendCatName, vendCat),]
    #Keeping this in case I can use it later
    #But I realized that the way I use the combinations of sic and cost that I need to do the categories the same way
    #This could maybe be used to find combinations when you do sic and cost seperatly
  }
}

valToName = function(num, data){
  
  num = as.numeric(as.character(num))
  for(i in 1:length(data)){
    if(num %in% data[[i]][[1]])
      return(data[[i]][[3]])
  }
}

nameToVal = function(name, data){
  
  for(i in 1:length(data)){
    if(name == data[[i]][[3]])
      return(data[[i]][[1]])
  }
  
}
