purchases = NULL
people = NULL

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

main = function(cardData, purchaseData){
  
  load(cardData, purchaseData)
  
  purchases2 = purchases[order(purchases$'EMPL ID'),] #note: not in true numerical order but not needed at this time
  
  result = matrix(nrow = length(levels(purchases2)), ncol = 6)
  cur = -1
  count = 1
  for(x in purchases2$'EMPL ID'){
    if(cur != x){
      cur = x
      jobAvg = avgAmtByJob(x)
      idAvg = avgAmtByID(x)
      result = rbind(result, c(x, calcScore(jobAvg, idAvg)))
    }
    cat(count / nrow(purchases2))
    count = count + 1
    if(count > 100)
      break
  }
  
  result = data.frame(result)
  colnames(result) = c("ID", "Amount Difference", "Transation Difference", "Retail Difference", "Wildcard Difference", "Total Score")
  result
}

calcScore = function(jobAvg, idAvg){
  
  amt = idAvg[[1]] - jobAvg[[1]]
  tran = idAvg[[2]] - jobAvg[[2]]
  retail = 0
  for(x in names(idAvg[[3]])){
    jobR = as.numeric(jobAvg[[3]][x])
    if(is.na(jobR))
      jobR = 0
    retail = retail + as.numeric(idAvg[[3]][x]) - jobR
  }
  wildcard = idAvg[[4]] - jobAvg[[4]]
  total = amt + tran + retail + wildcard
  c(amt, tran, retail, wildcard, total)
  
}