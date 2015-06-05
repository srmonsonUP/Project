library(tm)

textAnalysis = function(){
  
   level = read.csv("Level 3.csv")
  
   unique(level$ITEM.DESCRIPTION)
   
   count = tapply(level$EMPL.ID, level$ITEM.DESCRIPTION, function(x) length(x))
   
#    matrix(names(count), as.vector(count), ncol = 2)
#    
#    for(i in 1:nrow()){
#      deptVendComb = c(deptVendComb, c(rownames(count)[x[i,1]], colnames(count)[x[i,2]], count[x[i,1], x[i,2]]))
#    }

  aggregate(level$ITEM.DESCRIPTION, by = list(level$ITEM.DESCRIPTION), FUN = length)
}