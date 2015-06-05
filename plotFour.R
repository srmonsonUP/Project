library(ggplot2)

plotFour = function(){
  
  amtPerDept = amtPerDept()
  amtPerType = amtPerType()
  amtPerVend = amtPerVend()
  amtPerTypeVend = amtPerTypeVend()

  clearPlot()
  par(mfrow = c(2,2))
  
  plot(as.factor(amtPerDept[,1]), amtPerDept[,2], main = "Amount per Department")
  text(amtPerDept[,1],amtPerDept[,2]+50, round(amtPerDept[,2]), cex = 0.8)
  
  plot(as.factor(amtPerType[,1]), amtPerType[,2], main = "Amount per Cost Code")
  text(amtPerType[,1],amtPerType[,2]+500, round(amtPerType[,2]), cex = 0.8)
  
  plot(as.factor(amtPerVend[,1]), amtPerVend[,2], main = "Amount per SIC Code")
  text(amtPerVend[,1],amtPerVend[,2]+750, round(amtPerVend[,2]), cex = 0.8)
  
  plot(as.factor(amtPerTypeVend[,1]), amtPerTypeVend[,2], main = "Amount per Cost Code and SIC Code")
  text(amtPerTypeVend[,1],amtPerTypeVend[,2]+1000, round(amtPerTypeVend[,2]), cex = 0.8)
}