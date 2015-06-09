FormatTable <- function(in.table, title1.text, title2.text, title3.text,
                        h.offset, new.page){
  ##############################################################################
  ## Format data frame and output as nicely formatted table
  ##############################################################################
  table <- tableGrob(in.table, cols = colnames(in.table), show.colnames = TRUE,
                     show.rownames = FALSE, col.just = "center", 
                     core.just = "right")
  if(new.page == TRUE) {
    grid.newpage()
  }
  y.pos <- unit(0.5,"npc") + 0.5 * grobHeight(table)  ## top of table
  x.pos <- unit(0.0,"npc") + unit(h.offset, "in")
  line.height <- unit(c(1,2,3) * 12 * 1.3, "points") ## 12 == fontsize 
  if(title3.text == " "){ line.height <- line.height[c(3,1,2)] }
  # title.text <- rbind(title.text, "     ") ## blank line after title line
  title1 <- textGrob(title1.text, y = y.pos + line.height[3], # to raise height
                     x = x.pos, vjust = 0, hjust = 0, gp = gpar(fontsize = 12)) 
  title2 <- textGrob(title2.text, y = y.pos + line.height[2], # to raise height
                     x = x.pos, vjust = 0, hjust = 0, gp = gpar(fontsize = 12)) 
  title3 <- textGrob(title3.text, y = y.pos + line.height[1], # to raise height
                   x = x.pos, vjust = 0, hjust = 0, gp = gpar(fontsize = 12))
  gt <- gTree(children = gList(table, title1, title2, title3))
  grid.draw(gt)
} ## End of function FormatTable


