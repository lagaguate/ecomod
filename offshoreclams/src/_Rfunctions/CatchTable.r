
CatchTable <- function(fig.num, BBData, Sel.Bank){
  ##############################################################################
  ## catch and effort table, still using same data
  ##############################################################################
  temp <- aggregate(cbind(ROUND_CATCH / 1000, AREA  /1000000) ~Year, 
                            data = BBData, sum)
  ## fill in missing years
  yy <- seq(min(temp[, 1]), max(temp[, 1]))
  table.1 <- cbind(yy, matrix(rep(0, 2 * length(yy)), length(yy), 2))
  table.1[match(temp[, 1], table.1[, 1]), 2] <- temp[, 2]
  table.1[match(temp[, 1], table.1[, 1]), 3] <- temp[, 3]
  if(Sel.Bank == 1){
    survey.area <- 10908.1 ## area within 100m contour of Banquereau Bank
  }else if(Sel.Bank == 2){
    survey.area <- 49473.0 ## original survey area on Grand Bank
  }
  table.1  <- cbind(table.1, 100.0 * table.1[,3] / survey.area)
  table.1 <- cbind(table.1[,1], round(table.1[,2], 0),
                     round(table.1[,3], 1),
                     rep(0,length(yy)),
                     round(table.1[,4],2))
  idx <- which(table.1[,3] != 0)                  
  table.1[idx,4] <- round(table.1[idx,2] / table.1[idx,3], 0)
  colnames(table.1) <- c("Year", "Logged Catch (t)",
                            paste("Area Dredged (km", "\u00B2", ")", sep = ""),
                            "CPUE", "% Area")  
  return(as.data.frame(table.1))
} ## end of function CatchTable


