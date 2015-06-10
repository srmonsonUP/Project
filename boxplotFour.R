boxplotFour = function(){
  
  dep = boxPlotData(department)
  gdep = lapply(dep[,3], function(x) x)
  gdep = do.call(mapply, c(cbind, gdep))
  gdep$names = dep[,1]

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
  c = ""
  while(c == ""){
    clearPlot()
    par(mfrow = c(2,2), las = 2)
    
    amt = purchases[i, 'TRAN AMT']
    
    bxp(gdep, outline = F, main = "Transaction per Department", names = dep[,1])
    points(1:length(dep[,2]), as.vector(unlist(dep[,2])))

    
    bxp(gtyp, outline = F)
    points(1:length(typ[,2]), as.vector(unlist(typ[,2])))
    
    bxp(gven, outline = F)
    points(1:length(ven[,2]), as.vector(unlist(ven[,2])))
    
    bxp(gtypven, outline = F)
    points(1:length(typVen[,2]), as.vector(unlist(typVen[,2])))
    
    i = i + 1
    c = readline("Enter to cont")
  }
}

nameGen = function(len)
{
  n = c()
  for(i in 1:len)
    n = c(n, paste("Group ", LETTERS[floor(i/26)], LETTERS[i%%26], sep = ""))
  n
}