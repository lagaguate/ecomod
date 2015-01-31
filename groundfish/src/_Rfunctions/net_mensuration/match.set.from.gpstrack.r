match.set.from.gpstrack=function( DS="post.perley", net.root.dir ) {

  DS.saved = gsub(".redo$", "", DS)
  fn  = file.path( net.root.dir, "Scanmar", paste(DS.saved, "meta", "rdata", sep= "."))
  meta= NULL  
  
  if ( !grepl (".redo$", DS) ) {
    if(file.exists(fn)) load(fn)
    return(meta)
  }
    
  # Incorporation of newer data, combining timestamp
  pp=net_mensuration.db( DS=DS.saved, net.root.dir=net.root.dir ) 
  pp$lon=pp$longitude
  pp$lat=pp$latitude
  
  gf=groundfish.db(DS="gsinf")
  
  meta=data.frame(uniqueid=unique(pp$id), stringsAsFactors=FALSE )
  meta$sdate=NA
  meta$id=NA
  meta$bottom_temperature=NA
  meta$slon=NA
  meta$slat=NA
  meta$elon=NA
  meta$elat=NA
  meta$strat=NA
  meta$time.end=NA
  meta$min.distance = NA
 
  for(i in 1:nrow(meta)){
    k = meta$uniqueid[i]
    print(k)
    
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
        
        w = which.min(gfs$min.distance.test)
        if(gfs$min.distance.test[w]< 1 ){
          meta$id[i]=gfs$id[w]  # exact match with very high confidence
          meta$min.distance[i] = gfs$min.distance.test[w]
        } 
      }
    }
  }
  
  # fnn2 = "C:/cygwin64/home/mundenj/ecomod/groundfish/R/meta.rdata"
  # save( meta, file=fnn2)
  # load (fnn2)
  
  # Check for duplicates as some are data errors .. needed to be checked manually and raw data files altered
  # others are due to bad tows being redone ... so invoke a distance based rule as the correct one in gsinf (good tows only are recorded)
  dupids = unique( meta$id[ which( duplicated( meta$id, incomparables=NA) ) ] )
  for ( dups in dupids ) {
    uu = which(meta$id %in% dups)
    good = uu[ which.min( meta$min.distance[uu] ) ]
    notsogood = setdiff( uu, good )    
    meta$id[notsogood] = NA       
  }
  
  # redo the distance-based match to catch any that did not due to being duplicates above
  # does not seem to do much but kept for posterity
  
  unmatched = which( is.na(meta$id ) )
  if (length (unmatched) > 0) {
    for(i in unmatched ){
      
      k = meta$uniqueid[i]
      print(k)
      
      j = which(pp$id == k)
      if(length(j)>0) {
        ppc=pp[j,]
        m = which.min(ppc$timestamp)
        meta$sdate[i] = as.character(ppc$timestamp[m])
        dif = as.duration(ymd_hms(meta$sdate[i]) - gf$sdate)
        u = which(abs(dif)< dhours  (9)) 
        
        ## the next two lines are where things are a little different from above
        ## the catch all as yet unmatched id's only for further processing
        current.meta.ids = unique( sort( meta$id) )
        u = u[ which( ! (gf$id[u] %in% current.meta.ids ) )]
        
        if(length(u)> 1) {
          gfs=gf[u,]
          gfs$min.distance.test=NA
          
          for(v in 1:nrow (gfs)){
            distance.test = geodist(ppc[,c("lon","lat")], gfs[v,c("lon","lat")], method="great.circle")
            gfs$min.distance.test[v] = min(distance.test, na.rm=TRUE)
          }
          
          w = which.min(gfs$min.distance.test)
          if(gfs$min.distance.test[w]< 1 ){
            meta$id[i]=gfs$id[w]  # exact match with very high confidence
            meta$min.distance[i] = gfs$min.distance.test[w]
          } 
        }
      }
    }
  }
  
  
  ## now do a more fuzzy match based upon time stamps as there are no matches based upon distance alone
  
  nomatches = which( is.na( meta$id) )
  if (length(nomatches) > 1) {
    for(i in nomatches){
      k = meta$uniqueid[i]
      print(k)
      j = which(pp$id == k)
      if(length(j)>0) {
        ppc=pp[j,]
        m = which.min(ppc$timestamp)
        meta$sdate[i] = as.character(ppc$timestamp[m])
        dif = as.duration(ymd_hms(meta$sdate[i]) - gf$sdate)
        
        u = which( abs(dif)< dhours  (1) )
        if (length(u) == 1 ) { 
          current.meta.ids = unique( sort( meta$id) )
          u = u[ which( ! (gf$id[u] %in% current.meta.ids ) )]
          if (length(u) == 1 )   meta$id[i]= gfs$id[u]
        }          
      }
    }
  }    
 
   
  save(meta, file= fn, compress= TRUE)
 
}
