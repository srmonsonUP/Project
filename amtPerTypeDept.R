amtPerItemDept = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  
  purchases2 = purchases[order(purchases[,type]),]
  
  curType = -1
  curDept = -1
  for(x in purchases2[,type]){
    if(curType != x){
      curType = x
      t = purchases2[purchases2[,type] == curType, ]
      for(y in t$'EMPL ID'){
        
      }
    }
  }
}

deptByID = function(id){
  dep = as.character(people[people$'EMPL_ID' == id, department])[1]
  people[people[,department] == dep, ] #ids for said dept
}