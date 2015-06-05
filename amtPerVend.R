amtPerVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  avg = tapply(purchases[,'TRAN AMT'], purchases[,vendor], mean, na.rm = T)
  
  avgPerVend = as.data.frame(matrix(c(names(avg), as.vector(avg)), ncol = 2))
  
  categories = createCategories(na.omit(avgPerVend))
  names = as.factor(as.character(lapply(1:length(categories), function(x) categories[[x]][[3]])))
  values = as.numeric(as.character(lapply(1:length(categories), function(x) categories[[x]][[4]])))
  levels(names) = names
  
  plot(names, values, type = 'n', lty = NULL)
  text(names, values+500, round(values), cex=0.8)
  
  return(data.frame(names, values))
}