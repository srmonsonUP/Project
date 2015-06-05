amtPerDept = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  purchases2 = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  
  avg = tapply(purchases2$'TRAN AMT', purchases2[, department], mean, na.rm = T)
  
  avgPerDept = as.data.frame(matrix(c(names(avg), as.vector(avg)), ncol = 2))
  avgPerDept[,2] = as.numeric(as.character(avgPerDept[,2]))
  
  plot(as.factor(avgPerDept[,1]), avgPerDept[,2])
  text(avgPerDept[,1],avgPerDept[,2]+100, round(avgPerDept[,2]), cex = 0.8)
}