cluster = function(){
  
  if(!is.null(dev.list()))
    dev.off(dev.list()["RStudioGD"]) #clears plots
  
  purchases = read.csv("Purchasing Card Data.csv")
  colnames(purchases) =lapply(purchases[1,], as.character)
  purchases = purchases[-1,]
  
  sic = as.numeric(as.character(purchases$'SIC CODE'))
  costCode = as.numeric(as.character(purchases$'COST CODE'))
  
  hist(sic, breaks = length(unique(sic)))
  hist(costCode, breaks = length(unique(costCode)))
  
  type = "COST CODE"
  vendor = "SIC CODE"
  
  purchases = purchases[order(purchases$'COST CODE'),]
  
  avgPerType = c()
  cur = -1
  for(x in purchases$'COST CODE'){
    if(cur != x){
      cur = x
      avg = subset(purchases, purchases$'COST CODE' == x, select = 'TRAN AMT')
      avg = mean(as.numeric(as.character(avg$'TRAN AMT')), na.rm = T)
      avgPerType = c(avgPerType, x, avg)
    }
  }
  avgPerType = matrix(avgPerType, ncol = 2, byrow = T)
  plot(avgPerType[,1], avgPerType[,2], xlab = "COST Code", ylab = "Mean Amt", main = "Mean Amt / Cost Code")
  
  purchases = purchases[order(purchases$'COST CODE', purchases$'SIC CODE'),]
}