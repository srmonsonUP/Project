analyze = function(){
  
  sortBy = "COST CODE"
  
  data = read.csv("Purchasing Card Data.csv", header = F)[-1,]
  colnames(data) = lapply(data[1,], as.character)
  data = data[-1,]
  
  as.data.frame(tapply(as.numeric(data[,"TRAN AMT"]), data[,sortBy], mean))

}