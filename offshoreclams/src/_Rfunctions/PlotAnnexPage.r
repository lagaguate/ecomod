PlotAnnexPage <- function(){
  ##############################################################################
  ## Set up title page for appendix
  ##############################################################################
  op <- par(
            omi = c(1, 1, 1, 1),      ## Set outer margins with space at bottom
            mai = c(1, 0.25, 0.25, 0.25)  ## Set Figure margins (inches)
            )  
  plot(0:10, 0:10, type = "n", axes = FALSE, ann = FALSE)
  text.1 <- "Annex 1"
  text(5, 8, text.1, cex = 2.0)
  text.2 <- "Revised figures for public distribution"
  text(5, 7.2, text.2, cex = 2.0)
  par(op) ## reset parameters
} ## End of function PlotAnnexPage


