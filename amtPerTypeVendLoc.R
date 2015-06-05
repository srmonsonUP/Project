amtPerTypeVendLoc = function(){ #Not done
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  
  purchases2 = purchases[order(purchases[,type], purchases[,vendor], purchases[,location]),]
  
  curType = -1
  curVend = -1
  curLoc = -1
  avgPerTypeVendLoc = c()
  for(x in 1:nrow(purchases2)){
    if(curType != purchases2[x, type] || curVend != purchases2[x, vendor] || curLoc != purchases2[x, location]){
      curType = purchases2[x, type]
      curVend = purchases2[x, vendor]
      curLoc = purchases2[x, location]   
      
      avg = subset(purchases2, purchases2[,type] == curType & purchases2[,vendor] == curVend & purchases[,location] == curLoc, select = 'TRAN AMT')
      avg = mean(as.numeric(as.character(avg$'TRAN AMT')), na.rm = T)
      avgPerTypeVendLoc = c(avgPerTypeVendLoc, paste(curType, curVend, curLoc, sep = "."), avg)
    }
  }
  avgPerTypeVendLoc = matrix(avgPerTypeVendLoc, ncol = 2, byrow = T)
  plot(as.numeric(avgPerTypeVendLoc[,1]), as.numeric(avgPerTypeVendLoc[,2]), xlab = "Cost Code, SIC Code, State", ylab = "Mean Amt", main = "Mean Amt / (Cost Code, SIC Code State)")
}

tonum = function(type, vend, loc){
  
}