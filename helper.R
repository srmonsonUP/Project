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