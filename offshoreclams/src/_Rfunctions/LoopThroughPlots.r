LoopThroughPlots <- function (Sel.Bank, fig.num, table.num, log.data, lf.data){
  ##############################################################################
  ## Function to loop through Banquereau and Grand Bank 
  ## producing plots and tables
  ##############################################################################
  if(Sel.Bank == 1){
    bank.txt <- "Banquereau Bank"
    c.size <- 120
  }else if(Sel.Bank == 2){
    bank.txt <- "Grand Bank"
    c.size <- 105
  }
  ANNUAL <- CpuePlot(fig.num, log.data, Sel.Bank)   
  fig.num <- fig.num + 1
  BBData <- subset(log.data, log.data$BANK == Sel.Bank) ## subset selected bank
  out.table <- CatchTable(fig.num, BBData, Sel.Bank)
  ## Now output table of catch and effort
  table.1 <- out.table ## save numeric table as formatting will convert to text
  table.1[,2] <- prettyNum(table.1[,2], big.mark = ",")
  title1.text <- paste("Table ", table.num, 
                       ". Catch (t), footprint (area dredged km",
                       "\u00B2","), CPUE and percent of survey ", sep = "")
  title2.text <- paste("area by year for the offshore clam fishery on ", 
                       bank.txt, ".", sep = "")
  title.off <- 1.0
  if(Sel.Bank == 1){
    title3.text <- " "
  } else {
    title3.text <- "Values since 2006 are not representative of stock status."
  }
  FormatTable(table.1, title1.text, title2.text, title3.text, title.off,
              new.page = TRUE)
  table.num <- table.num + 1
  ## Plot of footprint by year
  Footprint(fig.num, out.table[,c(1, 2, 3)], Sel.Bank)
  fig.num <- fig.num + 1
  # plot of number of one minute squares fished per year
  oms.freq <- OMSFished(fig.num, BBData, Sel.Bank)
  fig.num <- fig.num + 1
  ## Add table of number of one minute squares fished by year
  
  oms.freq[, 2] <- prettyNum(oms.freq[, 2], big.mark = ",")
  title1.text = paste("Table ", table.num, ".  Number of one minute squares",
                      "fished per year in the offshore clam")
  title2.text = paste(" fishery on ", bank.txt, ".", sep = "")
  FormatTable(oms.freq, title1.text, title2.text, " ", 1.0, new.page = TRUE)
  table.num <- table.num + 1
  ## Plot map of distribution of fishing effort
  MapLogData(fig.num, log.data, Sel.Bank)
  fig.num <- fig.num + 1
  ## plot percent of large clams in catch
  Result <- PlotPercentLarge(fig.num, lf.data, Sel.Bank)
  out.table2 <- Result$Table
  fig.num <- fig.num + 1
  
  ## Table of percent large clams in catch
  table.3 <- cbind(as.numeric(rownames(out.table2)), out.table2)
  table.3[, 3] <- round(table.3[, 3], 2)
  table.3[, 2] <- prettyNum(table.3[, 2], big.mark = ",")
  title1.text =  " "
  title2.text = paste("Table ", table.num, ". Percent of large Arctic",
                      " Surfclams in unsorted catch on ", bank.txt, ".",
                      sep = "")
  title.off <- 1.0
  if(Sel.Bank == 2){ 
    title3.text =
          " Values since 2006 are not representative of stock status."
  } else {
    title3.text <- " "
  }
  ## new viewport for table on same page
  #pushViewport(viewport(x = unit(0.5, "npc"), y = unit(0.25, "npc"),
  #                      width = unit(5.5, "in"), height = unit(5, "in"),
  #                      just = "centre", gp = gpar(), clip = "inherit",
  #                      xscale = c(0, 1), yscale = c(0, 1),angle = 0))
  FormatTable(table.3, title1.text, title2.text, title3.text, title.off,
                new.page = TRUE)
  table.num <- table.num + 1 
  ## Plot the length frequencies
  PlotLengthFreq(fig.num, Result$P120, Result$p_mat, bank.txt, Result$Table, c.size)
  fig.num <- fig.num + 1
  return(list(ANNUAL = ANNUAL, table.1b = out.table))
} ## End of function LoopThroughPlots


