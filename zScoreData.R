zScoreData = function(sel){
  
  if(!checkLoad())
    load("Card Data.csv", "Purchasing Card Data.csv")
  
  clean()
  
  if(department %in% sel)
    purchases = merge(purchases, people, by.x = 'EMPL ID', by.y = 'EMPL_ID')
  
  
  if(length(sel) == 1){
    
    avg = tapply(purchases$'TRAN AMT', purchases[, sel], mean, na.rm = T)
    sd = tapply(purchases$'TRAN AMT', purchases[, sel], sd, na.rm = T)
    data = cbind(avg, sd)
    colnames(data) = c("mean", "sd")
    data = data[!is.na(data[,2]) & data[,2] != 0,]
    
  }else if(length(sel) == 2){
    
    avg = tapply(purchases$'TRAN AMT', list(purchases[, sel[1]], purchases[, sel[2]]), mean, na.rm = T)
    sd = tapply(purchases$'TRAN AMT', list(purchases[, sel[1]], purchases[, sel[2]]), sd, na.rm = T)
    
    x = which(avg > 0, arr.ind = T)
    avg = t(apply(x, 1, function(y) c(paste(rownames(avg)[y[1]], colnames(avg)[[y[2]]], sep = "."), avg[y[1], y[2]])))
    x = which(sd > 0, arr.ind = T)
    sd = t(apply(x, 1, function(y) c(paste(rownames(sd)[y[1]], colnames(sd)[[y[2]]], sep = "."), sd[y[1], y[2]])))
    
    data = suppressWarnings(merge(data.frame(avg), data.frame(sd), by = "X1")) #Maybe not a great idea to surpress this warning. But it's long
    colnames(data) = c("name", "sd")
    rownames(data) = data$name
    data = data[,-1]
    
  }
  
  hc = hclust(dist(data), method = "single")
  
  cat = cutree(hc, k = min(31, floor(sqrt(nrow(purchases) / 2)))) #Max groupings is apperently 32
  #TODO: figure out why I had to change this to 31
  data = merge.data.frame(data, cbind(names(cat), category = as.vector(cat)), by.x = "row.names", by.y = "V1")
  
  groups = tapply(data$Row.names, data$category, function(x) x)
  if(!department %in% sel)
    groups = lapply(groups, function(x) as.numeric(x))
  
  if(length(sel) == 1){
    avg = lapply(groups, function(x) mean(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T))
    sd = lapply(groups, function(x) sd(purchases[purchases[,sel] %in% x, 'TRAN AMT'], na.rm=T))
  }else if(length(sel) ==2){
    avg = lapply(groups, function(x) mean(purchases[as.numeric(paste(purchases[,sel[1]], purchases[,sel[2]], sep = ".")) %in% x, 'TRAN AMT'], na.rm=T))
    sd = lapply(groups, function(x) sd(purchases[as.numeric(paste(purchases[,sel[1]], purchases[,sel[2]], sep = ".")) %in% x, 'TRAN AMT'], na.rm=T))
  }
  
  cbind(groups, avg, sd)
}