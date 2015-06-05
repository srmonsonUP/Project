mergeLevel3toPurchases = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv", "Level 3.csv")
  
  clean()
  
  level$my.id = c(rep(NA, times = nrow(level)))
  purchases$my.id = c(rep(NA, times = nrow(purchases)))
  
  
  myid = 1
  for(i in 1:nrow(purchases)){
    id = purchases[i, 'EMPL ID']
    date = purchases[i, 'TRAN DATE']
    amt = purchases[i, 'TRAN AMT']
    
    equiv = level[level[,'EMPL.ID'] == id & level[,'TRAN.DATE'] == date & level[,'TRAN.AMT'] == amt, ]
    if(nrow(equiv) > 1)
      return(NULL) #Not sure what to do but it should be unique
    if(nrow(equiv) == 1){
      purchases[i,'my.id'] = myid
      qeuiv$my.id = myid
      myid = myid + 1
    }
    else{} 
    
    cat(paste(myid, " "))
  }
  
  merge(purchases, level, by = my.id)
}