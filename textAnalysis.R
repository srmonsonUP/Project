library(tm)

textAnalysis = function(){
  
   level = read.csv("Level 3.csv")
  
   unique(level$ITEM.DESCRIPTION)
   
   count = tapply(purchases2$'TRAN AMT', list(purchases2[,department], purchases2[,vendor]), function(x) length(x))
   
   
   
}