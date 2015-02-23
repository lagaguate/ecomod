numbersandweights.plots<-function(df1,showchemistry=FALSE){
    my.data <- df1[, colnames(df1)]
  
    numwt=TRUE
    abundbio=TRUE
    #showchemistry=TRUE
    
    #chart specifications
    weight<-c(2,2,2,2,0.5,0.5)
    type<-c(1,1,1,1,2,3)
    color<-c("chocolate1","cadetblue2","darkolivegreen3","orchid4","gray65","gray65")

    
    if (numwt){
      par(mai = c(1.25, 1.3, 1.13, 1.41))
      plot(df1$YEAR, df1$AVG_WEIGHT, type = "l", ylab = "AVG_WEIGHT", xlab = "YEAR", col=color[1], lty=type[1], lwd = weight[2])
      lines(df1$YEAR,df1$AVG_NUMBER, col=color[2], lty=type[2], lwd = weight[2])
      mtext("AVG_NUMBER", side = 4, line = 3)
      axis(side = 4, at=pretty(range(df1$AVG_NUMBER)))
      
      if (showchemistry){
          par(new=T)
          plot(df1$YEAR, df1$BOTTOM_TEMPERATURE, type = "l", ylab = "", xlab = "", axes=FALSE, col=color[5], lty=type[5], lwd = weight[5])
          par(new=T)
          plot(df1$YEAR, df1$BOTTOM_SALINITY, type = "l", ylab = "", xlab = "", axes=FALSE, col=color[6], lty=type[6], lwd = weight[6])
          
          legend('topright', 
                 c('AVG_WEIGHT','AVG_NUMBER','BOTTOM_TEMP','BOTTOM_SAL'),
                 lty=c(type[1:2],type[5:6]), 
                 lwd=c(weight[1:2],weight[5:6]),
                 col=c(color[1:2],color[5:6])
          )
      }else{
          legend('topright', 
                 c('AVG_NUMBER','AVG_WEIGHT'),
                 lty=type[1:2], 
                 lwd=weight[1:2],
                 col=color[1:2])
      }
}
    
    if (abundbio){
      par(mai = c(1.25, 1.3, 1.13, 1.41))
      plot(df1$YEAR, df1$BIOMASS, type = "l", ylab = "BIOMASS", xlab = "YEAR", col=color[3], lty=type[3], lwd = weight[3])
      par(new = TRUE)
      plot(df1$YEAR, df1$ABUNDANCE, type = "l", ylab = "", xlab = "", axes=FALSE, col=color[4], lty=type[4], lwd = weight[4])
      mtext("ABUNDANCE",side=4,line=4) 
      axis(4, at=pretty(range(df1$ABUNDANCE)), las=1)
      
      if (showchemistry){
          par(new=T)
          plot(df1$YEAR, df1$BOTTOM_TEMPERATURE, type = "l", ylab = "", xlab = "", axes=FALSE, col=color[5], lty=type[5], lwd = weight[5])
          par(new=T)
          plot(df1$YEAR, df1$BOTTOM_SALINITY, type = "l", ylab = "", xlab = "", axes=FALSE, col=color[6], lty=type[6], lwd = weight[6])
          
          legend('topright', 
                 c('BIOMASS','ABUNDANCE','BOTTOM_TEMP','BOTTOM_SAL'),
                 lty=c(type[3:4],type[5:6]), 
                 lwd=c(weight[3:4],weight[5:6]),
                 col=c(color[3:4],color[5:6])
          )
      }else{
          legend('topright', 
                 c('BIOMASS','ABUNDANCE'),
                 lty=type[3:4], 
                 lwd=weight[3:4],
                 col=color[3:4])
      }
    }  
} 