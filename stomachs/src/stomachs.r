
  
	loadfunctions( c( "stomachs", "taxonomy" ) )

  refresh.data
  if (refresh.data) {
    sdinf = stomach.db( "sdinf.redo" )
    sdtrips = stomach.db( "sdtrips.redo" )
    sddet = stomach.db( "sddet.redo" )
    sddigest = stomach.db( "sddigest.redo" )
    sdfullness = stomach.db( "sdfullness.redo" )
    sditem = stomach.db( "sditem.redo" )
    sdpred = stomach.db( "sdpred.redo" )
    sdsource = stomach.db( "sdsource.redo" )
    sdsto = stomach.db( "sdsto.redo" )
    sdtech = stomach.db( "sdtech.redo" )
  }
  
  tx = taxonomy.db("gstaxa")
  
  sddet = stomach.db( "sddet" )
  sdinf = stomach.db( "sdinf" )
  sditem = stomach.db( "sditem" )
  
  S = stomach.db( "sdsto" )
  S$fishid = paste( S$mission, S$setno, S$fshno )
  S = S[!duplicated(S$fishid) ,]
  S$n = 1
  S = merge (S, sdinf, by=c("mission", "setno"), all.x=T, all.y=F )
  S = merge (S, tx, by="spec", all.x=T, all.y=F )
  S$yr = as.numeric( as.character( years( S$sdate ) ))
  S$sp = paste( S$namecom, S$spec, sep="." )
  
  samples = xtabs( n ~ yr + sp, S )
  write.table( samples, file="clipboard", sep="," )
  
  
  
