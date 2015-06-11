boxPlotData = function(sel = vendor){  #Temporary initial value
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clean()
  
  if(department %in% sel)
    purchases = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  
  if(length(sel) == 1){
    
    avg = tapply(purchases$'TRAN AMT', purchases[, sel], mean, na.rm = T)
    med = tapply(purchases$'TRAN AMT', purchases[, sel], median, na.rm = T)
    q1 = tapply(purchases$'TRAN AMT', purchases[, sel], quantile, probs = 0.25, na.rm = T)
    q3 = tapply(purchases$'TRAN AMT', purchases[, sel], quantile, probs = 0.75, na.rm = T)
    data = cbind(avg, med, q1, q3)
    colnames(data) = c("mean", "median", "q1", "q3")
    
  }else if(length(sel) == 2){
    
    avg = tapply(purchases$'TRAN AMT', list(purchases[, sel[1]], purchases[, sel[2]]), mean, na.rm = T)
    med = tapply(purchases$'TRAN AMT', list(purchases[, sel[1]], purchases[, sel[2]]), median, na.rm = T)
    q1 = tapply(purchases$'TRAN AMT', list(purchases[, sel[1]], purchases[, sel[2]]), quantile, probs = 0.25, na.rm = T)
    q3 = tapply(purchases$'TRAN AMT', list(purchases[, sel[1]], purchases[, sel[2]]), quantile, probs = 0.75, na.rm = T) 
    
    x = which(avg > 0, arr.ind = T)
    avg = t(apply(x, 1, function(y) c(paste(rownames(avg)[y[1]], colnames(avg)[[y[2]]], sep = "."), avg[y[1], y[2]])))
    x = which(med > 0, arr.ind = T)
    med = t(apply(x, 1, function(y) c(paste(rownames(med)[y[1]], colnames(med)[[y[2]]], sep = "."), med[y[1], y[2]])))
    x = which(q1 > 0, arr.ind = T)
    q1 = t(apply(x, 1, function(y) c(paste(rownames(q1)[y[1]], colnames(q1)[[y[2]]], sep = "."), q1[y[1], y[2]])))
    x = which(q3 > 0, arr.ind = T)
    q3 = t(apply(x, 1, function(y) c(paste(rownames(q3)[y[1]], colnames(q3)[[y[2]]], sep = "."), q3[y[1], y[2]])))
    
    data = suppressWarnings(merge(data.frame(avg), data.frame(med), by = "X1")) #Maybe not a great idea to surpress this warning. But it's long
    data = suppressWarnings(merge(data, data.frame(q1), by = "X1"))             #and I don't really undertand it anyway
    data = suppressWarnings(merge(data, data.frame(q3), by = "X1"))
    colnames(data) = c("name", "mean", "median", "q1", "q3")
    rownames(data) = data$name
    data = data[,-1]

  }
  
  
  
  hc = hclust(dist(data), method = "single")
  
  cat = cutree(hc, k = min(32, floor(sqrt(nrow(purchases) / 2)))) #Max groupings is apperently 32
  
  data = merge.data.frame(data, cbind(names(cat), category = as.vector(cat)), by.x = "row.names", by.y = "V1")
  
  groups = tapply(data$Row.names, data$category, function(x) x)
  if(!department %in% sel)
    groups = lapply(groups, function(x) as.numeric(x))
  
  if(length(sel) == 1){
    avg = lapply(groups, function(x) mean(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T))
    box = lapply(groups, function(x) boxplot(purchases[purchases[,sel] %in% x, 'TRAN AMT'], plot=F, outline = F))
  }else if(length(sel) ==2){
    avg = lapply(groups, function(x) mean(purchases[as.numeric(paste(purchases[,sel[1]], purchases[,sel[2]], sep = ".")) %in% x, 'TRAN AMT'], na.rm=T))
    box = lapply(groups, function(x) boxplot(purchases[as.numeric(paste(purchases[,sel[1]], purchases[,sel[2]], sep = ".")) %in% x, 'TRAN AMT'], plot=F, outline = T))
  }
  
  cbind(groups, avg, box)
  
}