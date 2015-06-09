amtPerVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  avg = tapply(purchases[,'TRAN AMT'], purchases[,vendor], mean, na.rm = T)
  sd = tapply(purchases[,'TRAN AMT'], purchases[,vendor], sd, na.rm = T)
  
  avgPerVend = as.data.frame(matrix(c(names(avg), as.vector(avg)), ncol = 2))
  
  categories = createCategories(na.omit(avgPerVend), vendor)
  
  return(categories)
}