boxplotFour = function(){
  
  dep = boxPlotData(department)
  typ = boxPlotData(type)
  ven = boxPlotData(vendor)
#   typVen = boxPlotData(c(type, vendor)) Doesn't work yet
  
  clearPlot()
  par(mfrow = c(2,2), las = 2)

  tmp = lapply(dep[,3], function(x) x)
  tmp = do.call(mapply, c(cbind, tmp))
  bxp(tmp)

  tmp = lapply(typ[,3], function(x) x)
  tmp = do.call(mapply, c(cbind, tmp))
  bxp(tmp)

  tmp = lapply(ven[,3], function(x) x)
  tmp = do.call(mapply, c(cbind, tmp))
  bxp(tmp)



}