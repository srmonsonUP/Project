amtPerVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  avg = tapply(purchases[,'TRAN AMT'], purchases[,vendor], mean, na.rm = T)
  
  avgPerVend = as.data.frame(matrix(c(names(avg), as.vector(avg)), ncol = 2))
  
  categories = createCategories(na.omit(avgPerVend))
  
  return(categories)
}