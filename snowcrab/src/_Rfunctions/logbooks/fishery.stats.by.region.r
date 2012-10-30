 
  get.fishery.stats.by.region = function(landings0=NULL, Reg=NULL, y=NULL) {
    
    y = data.frame( yr=y )
    region = NULL
    
    if (is.null(Reg)) {
      Reg = "cfaall"
      region = unique(landings0$cfa)  # all data
    }
    if (Reg=="cfanorth") region = c("cfa20", "cfa21", "cfa22", "cfanorth", "north")
    if (Reg=="cfasouth") region = c("cfa23", "cfa24", "cfasouth", "cfaslope")
    if (Reg=="cfa4x") region = "cfa4x"
    
    if (is.null(landings0)) landings0 = landings.db()
    
    lnd = landings0[ which(landings0$cfa %in% region) ,]
     
    l = aggregate( lnd$landings, list(yr=lnd$yr), function(x) sum(x, na.rm=T))
    names(l) = c("yr", "landings")
    l = factor2number(l, c("yr", "landings"))
    rownames(l) = l[,1]
    
    out = merge(y, l, by="yr", all.x=T, all.y=F, sort=T)
    
    # these historical data seem to be off ... this is a temporary fix:
    # must bring in marfis data for the more recent period
#   if (Reg=="cfasouth") l$landings [ which(l$yr==2004)] = 7914
    # -- end of fix

    e = aggregate(lnd$effort, list(yr=lnd$yr), function(x) sum(x, na.rm=T))
    names(e) = c("yr", "effort")
    e = factor2number(e, c("yr", "effort"))
    
    out = merge(out, e, by="yr", all.x=T, all.y=F, sort=T)
    out$cpue = out$landings / out$effort 

    return(out)
    
  }


