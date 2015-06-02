library("rgl")

amtPerTypeVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  
  purchases2 = purchases[order(purchases[,type], purchases[,vendor]),]
  
  avgPerTypeVend = c()
  curType = -1
  curVend = -1
  for(x in 1:nrow(purchases2)){
    if(curType != purchases2[x, type] || curVend != purchases2[x, vendor]){
      curType = purchases2[x, type] #may need to convert to numeric
      curVend = purchases2[x, vendor]
      avg = subset(purchases2, purchases2[,type] == curType & purchases2[,vendor] == curVend, select = 'TRAN AMT')
      avg = mean(as.numeric(as.character(avg$'TRAN AMT')), na.rm = T)
      avgPerTypeVend = c(avgPerTypeVend, c(as.character(curType), as.character(curVend), avg))
    }
  }
  avgPerTypeVend = matrix(avgPerTypeVend, ncol = 3, byrow = T)
  plot(as.numeric(avgPerTypeVend[,1]), as.numeric(avgPerTypeVend[,3]), col = avgPerTypeVend[,2], xlab = "Cost Code", ylab = "Mean Amt")
  legend(x = "bottomright", legend = avgPerTypeVend[,2], col = avgPerTypeVend[,2])
}