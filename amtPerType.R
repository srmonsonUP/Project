amtPerType = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  avg = tapply(purchases[,'TRAN AMT'], purchases[,type], mean, na.rm = T)

  avgPerType = as.data.frame(matrix(c(names(avg), as.vector(avg)), ncol = 2))
  
  categories = createCategories(avgPerType, type)

  return(categories)
}


createCategories = function(data, selection){
  
  thresh = 0.05
  categories = c()

  count = 0
  while(nrow(data) > 0){
    val = as.numeric(as.character(data[1,2]))
    min = val - val * thresh
    max = val + val * thresh
    
    cat = data[as.numeric(as.character(data[,2])) >= min & as.numeric(as.character(data[,2])) <= max, ]
    data = subset(data, !(as.numeric(as.character(data[,2])) >= min & as.numeric(as.character(data[,2])) <= max))
    
    if(length(selection) == 2){
      val = mean(purchases[as.numeric(paste(purchases[,type], purchases[,vendor], sep = ".")) %in% cat[,1], ]$'TRAN AMT', na.rm = T)
      avgSD = sd(purchases[as.numeric(paste(purchases[,type], purchases[,vendor], sep = ".")) %in% cat[,1], ]$'TRAN AMT', na.rm = T)
    }else{
      val = mean(purchases[purchases[,selection] %in% cat[,1], ]$'TRAN AMT', na.rm = T)
      avgSD = sd(purchases[purchases[,selection] %in% cat[,1], ]$'TRAN AMT', na.rm = T)
    }
      
    cat = c(cat[,1:2], c(paste("Value ", LETTERS[floor(count/26)], LETTERS[count%%26 + 1], sep = ""), val, avgSD))
    count = count + 1
   
    
    categories = c(categories, list(cat))
  }
  categories
}