match.set.from.gpstrack=function( DS="post.perley", netswd=netswd ) {
  
  DS.saved = gsub(".redo$", "", DS)
  fn  = file.path(netswd, paste(DS.saved, "meta", "rdata", sep= "."))
  meta= NULL  
  
  if ( !grepl (".redo$", DS) ) {
    if(file.exists(fn)) load(fn)
    return(meta)
  }
    
  # Incorporation of newer data, combining timestamp
  pp=net_mensuration.db( DS=DS.saved, netswd=netswd ) 
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
  
## This is where I continue on
  # Check if there are any duplicates
  dupids = meta$id[ which( duplicated( meta$id, incomparables=NA) ) ]
  dups = meta[ which(meta$id %in% dupids), ]
  dups = dups[ order(dups$id), ]
  
  
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
        if (length(u) > 0 ) {
          ids.matched = meta$id[ !is.na(meta$id)]
          gfs = gf[u,] 
          gfs = gfs[ -which(gfs$id %in% ids.matched) ,]
          if (nrow( gfs) == 1 ) meta$id[i]= gfs$id
        }          
      }
    }
  }    
  
  # Re-check for duplicates
  dupids = meta$id[ which( duplicated( meta$id, incomparables=NA) ) ]
  dups = meta[ which(meta$id %in% dupids), ]
  dups = dups[ order(dups$id), ]
  
  
  
  du = which( duplicated( meta$id, incomparables=NA) )
  if (length(du)>0) {
    for (ee in du) {
      dd = which( meta$id %in% meta$id[ee] )
      tt = which.min( meta$min.distance[dd] )
      reject = setdiff( dd, tt )
      meta$id[reject] = NA
    }
  }
  
  dupids = meta$id[ which( duplicated( meta$id, incomparables=NA) ) ]
  dups = meta[ which(meta$id %in% dupids), ]
  dups = dups[ order(dups$id), ]
  
  nomatchids = which( is.na( meta$id) )
  nomatch = meta[ nomatchids,]
  nomatch = nomatch[ order(nomatch$uniqueid),]
  
  save(meta, file= fn, compress= TRUE)
  warning("No matches found for (nothing will follow if everything is OK:")
  print (nomatch)
  
  warning(" The following have duplicated matches:")
  print (dups)
  
}