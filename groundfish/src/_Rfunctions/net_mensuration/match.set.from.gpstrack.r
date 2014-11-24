match.set.from.gpstrack=function( DS="post.perley", netswd=netswd ) {
  fn= file.path(netswd, paste(DS, "meta", "rdata", sep= "."))
  meta= NULL  
  if(DS=="post.perley.saved") {
    fn= file.path(netswd, paste("post.perley", "meta", "rdata", sep= "."))
    if(file.exists(fn)) load(fn)
    return(meta)
  }
  
  # Incorporation of newer data, combining timestamp
  pp=net_mensuration.db( DS=DS, netswd=netswd ) 
  pp$lon=pp$longitude
  pp$lat=pp$latitude
  
  gf=groundfish.db(DS="gsinf")
  gf$sdate=with_tz(gf$sdate, "GMT") # Groundfish tables are stored in AST
  # gf$lon.end=-gf$lon.end (temporary fix until we correct gf)
  
  meta=data.frame(uniqueid=unique(pp$id))
  meta$sdate=NA
  meta$id=NA
  meta$bottom_temperature=NA
  meta$slon=NA
  meta$slat=NA
  meta$elon=NA
  meta$elat=NA
  meta$strat=NA
  meta$time.end=NA
  
  no.matches=NULL
  
  for(i in 1:nrow(meta)){
    print(i)
    k = meta$uniqueid[i]
    j = which(pp$id == k)
    if(length(j)>0) {
      ppc=pp[j,]
      
      m = which.min(ppc$timestamp)
      meta$sdate[i] = as.character(ppc$timestamp[m])
      dif = as.duration(ymd_hms(meta$sdate[i]) - gf$sdate)
      u = which(abs(dif)< dhours  (9) )
      
      if(length(u)> 1) {
        gfs=gf[u,]
        gfs$min.distance.test=NA
        
        for(v in 1:nrow (gfs)){
          distance.test = geodist(ppc[,c("lon","lat")], gfs[v,c("lon","lat")], method="great.circle")
          gfs$min.distance.test[v] = min(distance.test, na.rm=TRUE)
        }
        
        w= which.min(gfs$min.distance.test)
        if(gfs$min.distance.test[w]< 1 ){
          meta$id[i]=gfs$id[w]
        } else{
          no.matches=rbind(no.matches, meta[i,])
        }
        
      }
    }
  }
  save(meta, file= fn, compress= TRUE)
  print("no matches found for (nothing will follow if everything is OK:")
  return(no.matches)
}