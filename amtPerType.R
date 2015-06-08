amtPerType = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  avg = tapply(purchases[,'TRAN AMT'], purchases[,type], mean, na.rm = T)
  sd = tapply(purchases[,'TRAN AMT'], purchases[,type], sd, na.rm = T)

  avgPerType = as.data.frame(matrix(c(names(avg), as.vector(avg), as.vector(sd)), ncol = 3))
  
  categories = createCategories(avgPerType)
 

  return(categories)
}


createCategories = function(data){
  
  thresh = 0.05
  categories = c()

  count = 0
  while(nrow(data) > 0){
    val = as.numeric(as.character(data[1,2]))
    min = val - val * thresh
    max = val + val * thresh
    
    cat = data[as.numeric(as.character(data[,2])) >= min & as.numeric(as.character(data[,2])) <= max, ]
    avgSD = mean(as.numeric(as.character(cat[,3])), na.rm = T)              #TODO: check if this is what I should use
    cat = c(cat[,1:2], c(paste("Value ", LETTERS[floor(count/26)], LETTERS[count%%26 + 1], sep = ""), val, avgSD))
    count = count + 1
    data = subset(data, !(as.numeric(as.character(data[,2])) >= min & as.numeric(as.character(data[,2])) <= max))
    
    categories = c(categories, list(cat))
  }
  categories
}