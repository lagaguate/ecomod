PlotTitlePage <- function(Sel.Bank){
  ##############################################################################
  ## Set up title page
  ##############################################################################
  op <- par(
            omi = c(1, 1, 1, 1),      ## Set outer margins with space at bottom
            mai = c(5, 0.25, 0.25, 0.25)  ## Set Figure margins (inches)
           )  
  plot(0:10, 0:10, type = "n", axes = FALSE, ann = FALSE)
  text.1 <- "Offshore Surfclam Science Monitoring Program"
  text.2 <- "Commercial Stock Status Indicators for"
  text.3 <- "Arctic Surfclams on"
  text.4 <- "Banquereau Bank and Grand Bank"
  text.5 <- paste("Run on",format(Sys.Date(), "%d %b, %Y"))
  text(5, 5, text.1, cex = 1.2)
  text(5, 4.3, text.2, cex = 1.) 
  text(5, 3.7, text.3, cex = 1.) 
  text(5, 3.1, text.4, cex = 1.) 
  mtext(text.5, side = 1, line = 1, cex = 1., adj = 0, outer = TRUE) 
  par(op) ## reset parameters
} ## End of function PlotTitlePage


