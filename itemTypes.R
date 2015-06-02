itemTypes = function(){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clearPlot()
  
  sic = as.numeric(as.character(purchases$'SIC CODE'))
  costCode = as.numeric(as.character(purchases$'COST CODE'))
  
  hist(sic, breaks = length(unique(sic)))
  hist(costCode, breaks = length(unique(costCode)))
}