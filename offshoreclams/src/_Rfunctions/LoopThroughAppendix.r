LoopThroughAppendix <- function(Sel.Bank, bank.txt, cpue.table, 
                                annual.values, fig.num, table.num, log.data, lf.data){
  ##############################################################################
  ## Loop through banquereau and Grand Bank producing plots and tables for
  ## Appendix
  ##############################################################################
  table.1 <- cpue.table[ ,seq(1,4)]
  table.1[,2] <- prettyNum(table.1[,2], big.mark = ",")
  title1.text <- paste("Table ", table.num, 
                       ". Catch (t), Footprint (Area dredged km",
                       "\u00B2","), and CPUE by year  for the offshore ",
                       sep = "")
  title2.text <- paste("clam fishery on ", bank.txt, 
                       sep = "")
  FormatTable(table.1, title1.text, title2.text, " ", 1.0, new.page = TRUE)
  table.num <- table.num + 1
  AnnexCpuePlot(fig.num, annual.values, Sel.Bank, 0)
  ## AnnexCpuePlot(1, annual.values, 1, 1) ## normalized CPUE plot
  fig.num <- fig.num + 1
  BBData <- subset(log.data, log.data$BANK == Sel.Bank) ## subset selected bank
  out.table <- CatchTable(fig.num, BBData, Sel.Bank)
  Footprint(fig.num, out.table[,c(1, 2, 3)], Sel.Bank)
  fig.num <- fig.num + 1
  ## plot percent of large clams in catch
  Result <- PlotPercentLarge(fig.num, lf.data, Sel.Bank)
  out.table2 <- Result$Table
  fig.num <- fig.num + 1
  
  ## Table of percent large clams in catch
  table.3 <- cbind(as.numeric(rownames(out.table2)), out.table2)
  table.3[, 3] <- round(table.3[, 3], 2)
  table.3[, 2] <- prettyNum(table.3[, 2], big.mark = ",")
  title3.text =  " "
  title2.text =  " "
  title1.text = paste("Table ", table.num, ". Percent of large Arctic",
                      " Surfclams in unsorted catch on ", bank.txt, ".",
                      sep = "")
  if(Sel.Bank == 2){ 
    title3.text <- "Values since 2006 are not representative of stock status."
  }
  ## new viewport for table
  pushViewport(viewport(x = unit(0.5, "npc"), y = unit(0.25, "npc"),
                        width = unit(5.5, "in"), height = unit(5, "in"),
                        just = "centre", gp = gpar(), clip = "inherit",
                        xscale = c(0, 1), yscale = c(0, 1),angle = 0))
  FormatTable(table.3, title2.text, title1.text, title3.text, 1.0,
              new.page = TRUE)
}  ## End of function LoopThroughAppendix



