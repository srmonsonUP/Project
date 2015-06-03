amtPerItemDept = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  
#   purchases2 = purchases[order(purchases[,type]),]
  people2 = people[order(people[,department]),]
  
  curType = -1
  curDept = -1
  avgPerTypeDept = c()
  for(x in people2[, department]){
    if(curDept != x){
      curDept = x
      ids = people2[people[,department] == curDept, 'EMPL_ID']
      purchases2 = purchases[purchases$'EMPL ID' %in% ids, ]
     
      for(y in purchases2[,type]){
        if(curType != y){
          curType = y
          avg = subset(purchases, purchases2[,type] == y, select = 'TRAN AMT')
          avg = mean(as.numeric(as.character(avg$'TRAN AMT')), na.rm = T)
          avgPerTypeDept = c(avgPerTypeDept, as.character(curType), curDept, avg)
        }
      }
      curType = -1
    }
  }
  avgPerTypeDept = matrix(avgPerTypeDept, ncol = 3, byrow = T)
  plot(as.numeric(avgPerTypeDept[,1]), avgPerTypeDept[,3], col = as.numeric(as.factor(avgPerTypeDept[,2])), xlab = "Cost Code", ylab = "Mean Amt")
  legend(x = "right", legend = unique(avgPerTypeDept[,2]), col = as.numeric(unique(avgPerTypeDept[,2])), cex = .5)
}