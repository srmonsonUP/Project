amtPerDept = function(){
  require(ggplot2)
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  purchases2 = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  
  avg = tapply(purchases2$'TRAN AMT', purchases2[, department], mean, na.rm = T)
  sd = tapply(purchases2$'TRAN AMT', purchases2[, department], sd, na.rm = T)
  
  avgPerDept = as.data.frame(matrix(c(names(avg), as.vector(avg), as.vector(sd)), ncol = 3))
  avgPerDept[,2] = as.numeric(as.character(avgPerDept[,2]))
  avgPerDept[,3] = as.numeric(as.character(avgPerDept[,3]))
  
  return(avgPerDept)
}