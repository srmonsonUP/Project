purchases = NULL
people = NULL
jobCache = list()

type = "COST CODE"
vendor = "SIC CODE"
location = "VENDOR STATE"
department = "DEPARTMENT"

load = function(cardData, purchaseData){
  if(is.null(people) && !is.null(cardData)){
    people <<- read.csv(cardData)
  }
  
  if(is.null(purchases) && !is.null(purchaseData)){
    purchases <<- read.csv(purchaseData)
    colnames(purchases) <<- lapply(purchases[1,], as.character)
    purchases <<- purchases[-1,]
    purchases$'TRAN AMT' <<- as.numeric(as.character(purchases$'TRAN AMT'))
    purchases$'TRAN DATE' <<- as.Date(as.character(purchases$"TRAN DATE"), format = '%m/%d/%Y')
    purchases$'COST CODE' <<- as.numeric(as.character(purchases$'COST CODE'))
    purchases$'SIC CODE' <<- as.numeric(as.character(purchases$'SIC CODE'))
  }
}

checkLoad = function(){
  if(is.null(people) || is.null(purchases))
    return(FALSE)
  TRUE
}

clearPlot = function(){
  if(!is.null(dev.list()))
    dev.off(dev.list()["RStudioGD"]) #clears plots
}

clean = function(){
  
  purchases <<- purchases[complete.cases(purchases),]
  
  for(x in 1:nrow(purchases)){ 
    if(x > nrow(purchases))
      break
    if(purchases[x,'TRAN AMT'] < 0){
      amt = purchases[x, 'TRAN AMT']
      id = purchases[x, 'EMPL ID']
      purchases <<- purchases[-x,]
      purchases <<- subset(purchases, !(purchases[,'TRAN AMT'] == -1 *amt & purchases[, 'EMPL ID'] == id))
    }
  }
  
  purchases <<- subset(purchases, !(purchases[,vendor] == 5661))
  
}