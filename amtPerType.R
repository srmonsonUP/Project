amtPerType = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  
  type = "COST CODE"
  
  purchases2 = purchases[order(purchases[,type]),]
  
  avgPerType = c()
  cur = -1
  for(x in purchases2[,type]){
    if(cur != x){
      cur = x
      avg = subset(purchases, purchases2[,type] == x, select = 'TRAN AMT')
      avg = mean(as.numeric(as.character(avg$'TRAN AMT')), na.rm = T)
      avgPerType = c(avgPerType, x, avg)
    }
  }
  avgPerType = matrix(avgPerType, ncol = 2, byrow = T)
  plot(avgPerType[,1], avgPerType[,2], xlab = "Cost Code", ylab = "Mean Amt", main = "Mean Amt / Cost Code")
}