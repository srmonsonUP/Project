library("rgl")

amtPerTypeVend = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  clean()
 
  avg = tapply(purchases$'TRAN AMT', list(purchases[,type], purchases[,vendor]), mean, na.rm=T)
  
  x = which(avg > 0, arr.ind = T)
  
  avgPerTypeVend = c()
  for(i in 1:nrow(x)){
    avgPerTypeVend = c(avgPerTypeVend, c(rownames(avg)[x[i,1]], colnames(avg)[x[i,2]], avg[x[i,1], x[i,2]]))
  }
  avgPerTypeVend = matrix(avgPerTypeVend, ncol = 3, byrow = T)
  avgPerTypeVend[,1] = as.factor(avgPerTypeVend[,1])
  avgPerTypeVend[,2] = as.factor(avgPerTypeVend[,2])
#   plot3d(avgPerTypeVend, xlab = "SIC Code", ylab = "Cost Code", zlab = "Mean Amt")
  createCategories2(avgPerTypeVend)
}

createCategories2 = function(data){
  
  vendCat = createCategories(data[,c(1,3)])
  typeCat = createCategories(matrix(data[,2], data[,3], ncol = 2))
  
  while(nrow(data) > 0){
    vendCatName = getCatName(data[1,1], vendCat)
    typeCatName = getCatName(data[1,2], typeCat)
    
    cat = data[getCatName(data[,1], vendCat) == vendCatName & getCatName(data[,2], typeCat) == typeCatName]
    data = subset(data, !(getCatName(data[,1], vendCat) == vendCatName & getCatName(data[,2], typeCat) == typeCatName))
    
  }
}

