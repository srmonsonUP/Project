avgAmtByJob = function(job, cardData, purchaseData){
  
  people = read.csv(cardData, header = T)
  
  purchases = read.csv(purchaseData)
  colnames(purchases) = lapply(purchases[1,], as.character)
  purchases = purchases[c(-1,-2),]
  
  ids = subset(people, people$DEPARTMENT == job)$EMPL_ID #TODO: check department is the field I want
  #TODO: ensure I care about case sensitivity
  
  amounts = subset(purchases, is.element(purchases$'EMPL ID', ids))$'TRAN AMT'
  m = mean(as.numeric(amounts), na.rm = T)
  
  
  
}