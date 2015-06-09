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
  plot(hc)
  
  cat = cutree(hc, k = floor(sqrt(nrow(purchases) / 2)))
  
  data = merge.data.frame(data, cbind(names(cat), category = as.vector(cat)), by.x = "row.names", by.y = "V1")
  
  groups = tapply(data$Row.names, data$category, function(x) x)
  groups = lapply(groups, function(x) as.numeric(x))
  
  avg = lapply(groups, function(x) mean(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T))
  med = lapply(groups, function(x) median(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T))
  q1 = lapply(groups, function(x) quantile(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T, probs = 0.25))
  q3 = lapply(groups, function(x) quantile(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T, probs = 0.75))
  
  x
  
}