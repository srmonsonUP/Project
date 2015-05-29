avgAmtByJob = function(job, cardData, purchaseData){
  
  people = read.csv(cardData, header = T)
  
  purchases = read.csv(purchaseData)
  colnames(purchases) = lapply(purchases[1,], as.character)
  purchases = purchases[c(-1,-2),]
  
  ids = subset(people, people$DEPARTMENT == job)$EMPL_ID #TODO: check department is the field I want
  #TODO: ensure I care about case sensitivity
  
  amounts = subset(purchases, !is.na(match(purchases$"EMPL ID",ids)))
  for(x in levels(amounts$'EMPL ID')){
    if(!x %in% ids)
      y
  }
  m = mean(as.numeric(amounts$'TRAN AMT'), na.rm = T)
  
  f = mean(tapply(amounts$'EMPL ID', amounts$'EMPL ID', length), na.rm = T)
 
  c(m, f)
  
  
}