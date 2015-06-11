zScoreFour = function(){
  
  dep = zScoreData(department)
  typ = zScoreData(type)
  ven = zScoreData(vendor)
  typVen = zScoreData(c(type, vendor))
  
  
  purchases = merge(purchases, people, by.x = "EMPL ID", by.y = "EMPL_ID")
  i = 1
  cont = ""
  check = F
  j = 0
  for(i in 1:nrow(purchases)){
    
#     clearPlot()
#     par(mfrow = c(2,2), las = 2)
    
    amt = purchases[i, 'TRAN AMT']
  
#     plot(1:length(dep[,1]), dep[,2], xaxt = "n", main = "Amt per Department", xlab = "Dept", ylab = "Average Amount")
#     axis(1, at = 1:length(dep[,1]), labels = lapply(dep[,1], function(x) strsplit(x, split=" ")[[1]][1][1]))
    group = lapply(dep[,1], function(x) purchases[i, department] %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(is.na(group)){
#       usr = par("usr")
#       text(usr[1], usr[4], "No SD available", col = 'blue', cex = 1.5, adj = c(0,1))
    }else{
      z = (amt - dep[group, 2][[1]]) / dep[group,3][[1]]
      if(z > 1.96 || z < -1.96){
        check = T
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'red', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'red', pch = 15, cex = 1.5)
      }else{
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'green', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'green', pch = 15, cex = 1.5)
      }
    }
        
    
#     plot(1:length(typ[,1]), typ[,2], xaxt = 'n', main = "Amt per Cost Code", xlab = "Cost Code Group", ylab = "Average Amount")
#     axis(1, at = 1:length(typ[,1]), labels = nameGen(length(typ[,1])))
    group = lapply(typ[,1], function(x) purchases[i, type] %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(is.na(group)){
#       usr = par("usr")
#       text(usr[1], usr[4], "No SD available", col = 'blue', cex = 1.5, adj = c(0,1))
    }else{
      z = (amt - typ[group, 2][[1]]) / typ[group,3][[1]]
      
      if(z > 1.96 || z < -1.96){
        check = T
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'red', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'red', pch = 15, cex = 1.5)
      }else{
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'green', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'green', pch = 15, cex = 1.5)
      }
    }
    
    
#     plot(1:length(ven[,1]), ven[,2], xaxt = 'n', main = "Amt per SIC Code", xlab = "SIC Code Group", ylab = "Average Amount")
#     axis(1, at = 1:length(ven[,1]), labels = nameGen(length(ven[,1])))
    group = lapply(ven[,1], function(x) purchases[i, vendor] %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(is.na(group)){
#       usr = par("usr")
#       text(usr[1], usr[4], "No SD available", col = 'blue', cex = 1.5, adj = c(0,1))
    }else{
      z = (amt - ven[group, 2][[1]]) / ven[group,3][[1]]
     
      if(z > 1.96 || z < -1.96){
        check = T
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'red', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'red', pch = 15, cex = 1.5)
      }else{
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'green', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'green', pch = 15, cex = 1.5)
      }
    }
    
    
#     plot(1:length(typVen[,1]), typVen[,2], xaxt = 'n', main = "Amt per Cost,SIC Code", xlab = "Cost,SIC Code Group", ylab = "Average Amount")
#     axis(1, at = 1:length(typVen[,1]), labels = nameGen(length(typVen[,1])))
    group = lapply(typVen[,1], function(x) paste(purchases[i, type], purchases[i, vendor], sep = ".") %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(is.na(group)){
#       usr = par("usr")
#       text(usr[1], usr[4], "No SD available", col = 'blue', cex = 1.5, adj = c(0,1))
    }else{
      z = (amt - typVen[group, 2][[1]]) / typVen[group,3][[1]]
      
      if(z > 1.96 || z < -1.96){
        check = T
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'red', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'red', pch = 15, cex = 1.5)
      }else{
#         usr = par("usr")
#         text(usr[1], usr[4], paste("Z score is", round(z, 2)), col = 'green', cex = 1.5, adj = c(0,1))
#         points(group, amt, col = 'green', pch = 15, cex = 1.5)
      }
    }
    
    
    
#     i = i + 1
    if(check){
#       cont = readline(paste("Enter to continue", i))
      check = F
      j = j + 1
    }
    cat("\014")
    cat(round(i * 100 / nrow(purchases), 2))
  }
    
  round(j * 100 / nrow(purchases), 2)
} #TODO check why z score is the same between graphs 2 and 4