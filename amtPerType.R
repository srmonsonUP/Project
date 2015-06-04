amtPerType = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  avg = tapply(purchases[,'TRAN AMT'], purchases[,type], mean, na.rm = T)

  avgPerType = as.data.frame(matrix(c(names(avg), as.vector(avg)), ncol = 2))
#   plot(as.factor(avgPerType[,1]), as.numeric(as.character(avgPerType[,2])), xlab = "Cost Code", ylab = "Mean Amt", main = "Mean Amt / Cost Code")
 
  
  categories = createCategories(avgPerType)
  names = as.factor(as.character(lapply(1:length(categories), function(x) categories[[x]][[3]])))
  values = as.numeric(as.character(lapply(1:length(categories), function(x) categories[[x]][[4]])))
  levels(names) = names
  plot(names, values, type = 'n', lty = NULL)
  text(names, values+100, round(values), cex=0.8)
}


createCategories = function(data){
  
  thresh = 0.05
  categories = c()
  
#   if(!is.numeric(data[,2])){                              I tried to do this to reduce
#     data[,2] = as.numeric(as.character(data[,2]))         the conversions below
#   }                                                       but it didn't seem to work
  
  count = 0
  while(nrow(data) > 0){
    val = as.numeric(as.character(data[1,2]))
    min = val - val * thresh
    max = val + val * thresh
    
    cat = data[as.numeric(as.character(data[,2])) >= min & as.numeric(as.character(data[,2])) <= max, ]
    cat = c(cat, c(paste("Value ", LETTERS[floor(count/26)], LETTERS[count%%26 + 1], sep = ""), val))
    count = count + 1
    data = subset(data, !(as.numeric(as.character(data[,2])) >= min & as.numeric(as.character(data[,2])) <= max))
    
    if(cat[[4]] < 50000) #Temporarily remove outlier
      categories = c(categories, list(cat))
  }
  categories
  
}