deptVendComb = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clean()
  
  purchases2 = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  
  count = tapply(purchases2$'TRAN AMT', list(purchases2[,department], purchases2[,vendor]), function(x) length(x))
  
  x = which(count > 0, arr.ind = T)
  
  deptVendComb = c()
  for(i in 1:nrow(x)){
    deptVendComb = c(deptVendComb, c(rownames(count)[x[i,1]], colnames(count)[x[i,2]], count[x[i,1], x[i,2]]))
  }
  deptVendComb = matrix(deptVendComb, ncol = 3, byrow = T)
  
  deptVendComb[order(deptVendComb[,1], -xtfrm(as.numeric(deptVendComb[,3]))),]
}