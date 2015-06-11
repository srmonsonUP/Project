boxplotFour = function(){
  
  dep = boxPlotData(department)
  gdep = lapply(dep[,3], function(x) x)
  gdep = do.call(mapply, c(cbind, gdep))
  gdep$names = lapply(dep[,1], function(x) strsplit(x, split=" ")[[1]][1][1])

  typ = boxPlotData(type)
  gtyp = lapply(typ[,3], function(x) x)
  gtyp = do.call(mapply, c(cbind, gtyp))
  gtyp$names = nameGen(length(typ[,1]))

  ven = boxPlotData(vendor)
  gven = lapply(ven[,3], function(x) x)
  gven = do.call(mapply, c(cbind, gven))
  gven$names = nameGen(length(ven[,1]))
  
  typVen = boxPlotData(c(type, vendor))
  gtypven = lapply(typVen[,3], function(x) x)
  gtypven = do.call(mapply, c(cbind, gtypven))
  gtypven$names = nameGen(length(typVen[,1]))

  
  purchases = merge(purchases, people, by.x = "EMPL ID", by.y = "EMPL_ID")
  i = 1
  cont = ""
  check = F
  j = 0
  for(i in 1:nrow(purchases)){
#     clearPlot()
#     par(mfrow = c(2,2), las = 2)
#     c = 'green'
    
    amt = purchases[i, 'TRAN AMT']
    
#     bxp(gdep, outline = F, main = "Transaction per Department", names = dep[,1])
#     points(1:length(dep[,2]), as.vector(unlist(dep[,2])))
    group = lapply(dep[,1], function(x) purchases[i, department] %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(amt < dep[group, 3][[1]]$stats[2,1]){  #First Quartile
      check = T
#       c = 'orange'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(dep[group, 3][[1]]$stats[2,1] - amt, 2), "under first quartile"), col = c, cex = 1.5, adj = c(0,1))
    } 
    if(amt > dep[group, 3][[1]]$stats[4,1]){  #Third Quartile
      check = T
#       c = 'red'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(amt - dep[group, 3][[1]]$stats[4,1], 2), "over third quartile"), col = c, cex = 1.5, adj = c(0,1))
    }   
#     points(group, amt, col = c, cex = 1.5, pch = 15)
#     c = 'green'
    
    
#     bxp(gtyp, outline = F, main = "Amount Per Cost Code")
#     points(1:length(typ[,2]), as.vector(unlist(typ[,2])))
    group = lapply(typ[,1], function(x) purchases[i, type] %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(amt < typ[group, 3][[1]]$stats[2,1]){  #First Quartile
      check = T
#       c = 'orange'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(typ[group, 3][[1]]$stats[2,1] - amt, 2), "under first quartile"), col = c, cex = 1.5, adj = c(0,1))
    } 
    if(amt > typ[group, 3][[1]]$stats[4,1]){  #Third Quartile
      check = T
#       c = 'red'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(amt - typ[group, 3][[1]]$stats[4,1], 2), "over third quartile"), col = c, cex = 1.5, adj = c(0,1))
    }   
#     points(group, amt, col = c, cex = 1.5, pch = 15)
#     c = 'green'
    
    
#     bxp(gven, outline = F, main = "Amount per SIC Code")
#     points(1:length(ven[,2]), as.vector(unlist(ven[,2])))
    group = lapply(ven[,1], function(x) purchases[i, vendor] %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(amt < ven[group, 3][[1]]$stats[2,1]){  #First Quartile
      check = T
#       c = 'orange'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(ven[group, 3][[1]]$stats[2,1] - amt, 2), "under first quartile"), col = c, cex = 1.5, adj = c(0,1))
    } 
    if(amt > ven[group, 3][[1]]$stats[4,1]){  #Third Quartile
      check = T
#       c = 'red'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(amt - ven[group, 3][[1]]$stats[4,1], 2), "over third quartile"), col = c, cex = 1.5, adj = c(0,1))
    }   
#     points(group, amt, col = c, cex = 1.5, pch = 15)
#     c = 'green'
    
    
#     bxp(gtypven, outline = F, main = "Amount per Cost,SIC Code")
#     points(1:length(typVen[,2]), as.vector(unlist(typVen[,2])))
    group = lapply(typVen[,1], function(x) as.numeric(paste(purchases[i, type], purchases[i, vendor], sep = ".")) %in% x)
    group = which(group == T, arr.ind = T)[1]
    if(amt < typVen[group, 3][[1]]$stats[2,1]){  #First Quartile
      check = T
#       c = 'orange'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(typVen[group, 3][[1]]$stats[2,1] - amt, 2), "under first quartile"), col = c, cex = 1.5, adj = c(0,1))
    } 
    if(amt > typVen[group, 3][[1]]$stats[4,1]){  #Third Quartile
      check = T
#       c = 'red'
#       usr = par("usr")
#       text(usr[1], usr[4], paste("Amount is", round(amt - typVen[group, 3][[1]]$stats[4,1], 2), "over third quartile"), col = c, cex = 1.5, adj = c(0,1))
    }   
#     points(group, amt, col = c, cex = 1.5, pch = 15)
#     c = 'green'
    
    
#     i = i + 1
    if(check){
#       cont = readline(paste("Enter to cont to", i))
      check = F
      j = j + 1
    }
      cat("\014")
      cat(round(i * 100 / nrow(purchases), 2))
  }
  round(j * 100 / nrow(purchases),2)
}
