analyze = function(){
  
  sortBy = "COST CODE"
  
  data = read.csv("Purchasing Card Data.csv", header = F)[-1,]
  colnames(data) = lapply(data[1,], as.character)
  data = data[-1,]
  
  r = matrix(nrow = length(levels(data[,sortBy])))
  
  i = 0
  for(x in levels(data[, sortBy])){
    d = subset(data, x == sortBy) 
    i = i + 1
  }
    
  
}