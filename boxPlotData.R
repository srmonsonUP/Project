boxPlotData = function(sel = vendor){  #Temporary initial value
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clean()
  resetPar()
  
  if(sel == department)
    purchases = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID') #check if overwrite or creates local
  
  avg = tapply(purchases$'TRAN AMT', purchases[, sel], mean, na.rm = T)
  med = tapply(purchases$'TRAN AMT', purchases[, sel], median, na.rm = T)
  q1 = tapply(purchases$'TRAN AMT', purchases[, sel], quantile, probs = 0.25, na.rm = T)
  q3 = tapply(purchases$'TRAN AMT', purchases[, sel], quantile, probs = 0.75, na.rm = T)
  
  data = cbind(avg, med, q1, q3)
  
  hc = hclust(dist(data), method = "single")
  
  cat = cutree(hc, k = min(32, floor(sqrt(nrow(purchases) / 2)))) #Max groupings is apperenlty 32
  
  data = merge.data.frame(data, cbind(names(cat), category = as.vector(cat)), by.x = "row.names", by.y = "V1")
  
  groups = as.character(tapply(data$Row.names, data$category, function(x) x))
  if(sel != department)
    groups = lapply(groups, function(x) as.numeric(x))
  
  avg = lapply(groups, function(x) mean(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T))
  box = lapply(groups, function(x) boxplot(purchases[purchases[,sel] %in% x, 'TRAN AMT'], plot=F, outline = F))
#   box = lapply(box, function(x) x[-4]) #Removes mean?
  cbind(groups, avg, box)
  
}