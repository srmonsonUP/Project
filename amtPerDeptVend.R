amtPerDeptVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  purchases2 = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  
  avg = tapply(purchases2$'TRAN AMT', list(purchases2[,department], purchases2[,vendor]), mean, na.rm = T)
  
  x = which(avg > 0, arr.ind = T)
  
  avgPerDeptVend = c()
  for(i in 1:nrow(x)){
    avgPerDeptVend = c(avgPerDeptVend, c(rownames(avg)[x[i,1]], colnames(avg)[x[i,2]], avg[x[i,1], x[i,2]]))
  }
  avgPerDeptVend = matrix(avgPerDeptVend, ncol = 3, byrow = T)
  
  avgPerDeptVend[,1] = paste(avgPerDeptVend[,1], ".", avgPerDeptVend[,2], sep = "")
  avgPerDeptVend[,2] = avgPerDeptVend[,3]
  avgPerDeptVend = avgPerDeptVend[,-3]
  
  categories = createCategories(as.data.frame(avgPerDeptVend))
  names = as.factor(as.character(lapply(1:length(categories), function(x) categories[[x]][[3]])))
  values = as.numeric(as.character(lapply(1:length(categories), function(x) categories[[x]][[4]])))
  levels(names) = names
  plot(names, values, type = 'n', lty = NULL)
  text(names, values+500, round(values), cex=0.8)
  
}