amtPerType = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
  
  purchases2 = purchases[order(purchases[,type]),]
  
  avgPerType = c()
  cur = -1
  
  avg = tapply(purchases2[,'TRAN AMT'], purchases2[,type], mean, na.rm = T)

  avgPerType = matrix(c(names(avg), as.vector(avg)), ncol = 2)
  plot(as.factor(avgPerType[,1]), avgPerType[,2], xlab = "Cost Code", ylab = "Mean Amt", main = "Mean Amt / Cost Code")
}