
  # general data processing functions for the 
  # ecansap data set

  cleanup.set = function(set) {
    set$yr = 1900 + set$yr 
    set$sa = 1  # dummy value for plotting purposes
 
    # large values evident after histogram: set as NA; try:  hist(set$grd, "FD",xlim=c(0,3000))
    bad.sets = which( set$pel>100 | set$grd>5000 | set$shark>300 | set$allcaught>7000 | set$pred1>3000 | set$pred2>6000 | set$prey>100 )
    set = set[-bad.sets,]
    
    set$pd = set$pel / set$grd
    
    q = log10(set$grd); q[!is.finite(q)]=NA; set$grd_scaled=scale(q, center=T, scale=T)
    q = log10(set$pel); q[!is.finite(q)]=NA; set$pel_scaled=scale(q, center=T, scale=T)
   
    r = min( c(set$pel_scaled, set$grd_scaled), na.rm=T)
    
    set$grd_scaled =  set$grd_scaled - r
    set$pel_scaled =  set$pel_scaled - r
     
    set$pd_anom = set$pel_scaled / (set$pel_scaled + set$grd_scaled)
     
    return (set)

  }

  # -------------------------------------
  
  get.cat.ecnasap = function(source="file") {
    if      (source=="file") cat = dbGetQuery( ec, statement = "select * from xall" )
    else if (source=="redo") {
      dbSendQuery(ec, 'drop table if exists xall')
      dbSendQuery(ec, paste(
              'create table xall',
              'select i.*, c.spid, c.totno,',
              '       s.code, s.commonname, s.scientificname, ',
              '       s.pelagic, s.elasmo, s.mammal, s.invert, ',
              '       s.pred1, s.pred2, s.prey',
              'from inf as i',
              'left join cat as c on i.setid = c.setid',
              'left join spec_classified as s on c.spid = s.spid',
              'where c.totno < 50000 and i.yr <= 1994'
              ))
      dbSendQuery(ec, "alter table xall add index (setid) " )
      cat = dbGetQuery( ec, statement = "select * from xall" )
    }
    

    return (cat)
  }
	
	
  make.db = function(db, name, t0) {
    dbWriteTable(db, name, t0, overwite=T, row.names=F)
  }

  copy.db = function(db, old, new) {
    dbSendQuery(db, paste('drop table if exists', new))
    dbSendQuery(db, paste('create table', new, 'select * from ', old ))
  }

  merge.db = function(db, base, toadd, sql) {
    # base is the base table name
    # add is the tmp table *frame* that is to be merged into t0
    dbSendQuery(db, paste( 'drop table if exists toadd' ) )
    dbWriteTable(db, 'toadd', toadd, overwite=T, row.names=F)
    
    dbSendQuery(db, paste( 'drop table if exists basecopy' ) )
    dbSendQuery(db, paste( 'create table basecopy', sql ))
    
    dbSendQuery(db, paste( 'drop table if exists', base ) )
    dbSendQuery(db, paste( 'create table', base, 'select * from basecopy' ))

    dbSendQuery(db, paste( 'drop table if exists basecopy' ) )
    dbSendQuery(db, paste( 'drop table if exists toadd' ) )
    
 }

# -----------------------------------

  counttaxa = function(q) { 
    r = unique(q)
    r = r[is.finite(r)]
    return( length( r ) )
  }
  
# -----------------------------------

  ecnasap.catches = function (x=NULL, source="file") {

    set = NULL

    if (source=="file") set = dbGetQuery( ec, statement = "select * from base" )
 
    if (source=="redo") {
     
      # convoluted as the set id's bmust be the same sequence:
   
   
      # all fish   
        m = x[,c("setid","spid", "totno")]
        allcaught = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        ntaxa = tapply( m$spid, m$setid, counttaxa, simplify=T ) 
     
      # groundfish
        m = x
        i = which( m$pelagic==1 | m$elasmo==1 | m$invert==1 | m$mammal ==1 ) 
        m[ i, c("spid", "totno")] = c(NA, 0)
        grd = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        ngrd = tapply( m$spid, m$setid, counttaxa, simplify=T ) 
        i = NULL
        
      # pelagic            
        m = x
        i = which(m$pelagic != 1)
        m[ i, c("spid", "totno")] = c(NA, 0)
        pel = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        npel = tapply( m$spid, m$setid, counttaxa, simplify=T ) 
        i = NULL

      # sharks
        m = x
        i = which(m$elasmo != 1)
        m[ i, c("spid", "totno")] = c(NA, 0)
        shark = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        nshark =  tapply( m$spid, m$setid, counttaxa, simplify=T ) 
        i = NULL

      # inverts
        m = x
        i = which(m$invert != 1)
        m[ i, c("spid", "totno")] = c(NA, 0)
        invert = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        ninvert = tapply( m$spid, m$setid, counttaxa, simplify=T ) 
        i = NULL

      # pred1
        m = x
        i = which(m$pred1 != 1)
        m[ i, "totno" ] = 0
        pred1 = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        i = NULL
      
      # pred2
        m = x
        i = which(m$pred2 != 1)
        m[ i, "totno" ] = 0
        pred2 = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        i = NULL

      # prey 
        m = x
        i = which(m$prey != 1)
        m[ i, "totno" ] = 0
        prey = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
        i = NULL

      # individual species
        m = x
        m[ which(m$spid != 23) , "totno" ] = 0
        cod = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 150) , "totno" ] = 0
        haddock = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 
       
        m = x
        m[ which(m$spid != 94) , "totno" ] = 0
        capelin = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 14) , "totno" ] = 0
        acod = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 10) , "totno" ] = 0
        amPlaice = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 34) , "totno" ] = 0
        atSpiny.lumpsucker = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 174) , "totno" ] = 0
        loScuplin = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 201) , "totno" ] = 0
        noSandlance = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 265) , "totno" ] = 0
        spDogfish = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 271) , "totno" ] = 0
        thSkate = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 276) , "totno" ] = 0
        wiFlounder = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 278) , "totno" ] = 0
        yeFlounder = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

        m = x
        m[ which(m$spid != 136) , "totno" ] = 0
        frStargazer = tapply( m$totno, m$setid, function(q) { sum( q, na.rm=T ) }, simplify=T ) 

     
      catches = as.data.frame( cbind( 
             setid2=as.numeric(names(allcaught)), 
             ntaxa = as.vector(ntaxa),
             ngrd = as.vector(ngrd),
             npel = as.vector(npel),
             nshark = as.vector(nshark),
             ninvert = as.vector(ninvert),
             allcaught=as.vector(allcaught), 
             grd=as.vector(grd), 
             pel=as.vector(pel), 
             shark=as.vector(shark), 
             invert=as.vector(invert), 
             pred1=as.vector(pred1),
             pred2=as.vector(pred2), 
             prey=as.vector(prey),
             cod=as.vector(cod),
             haddock=as.vector(haddock),
             capelin=as.vector(capelin),
             acod=as.vector(acod),
             amPlaice=as.vector(amPlaice),
             atSpiny.lumpsucker=as.vector(atSpiny.lumpsucker),
             loScuplin=as.vector(loScuplin),
             noSandlance=as.vector(noSandlance),
             spDogfish=as.vector(spDogfish),
             thSkate=as.vector(thSkate),
             wiFlounder=as.vector(wiFlounder),
             yeFlounder=as.vector(yeFlounder),
             frStargazer=as.vector(frStargazer)
             ) )

      dbSendQuery(ec, paste( 'drop table if exists base' ) )
      dbSendQuery(ec, paste( 'create table base select * from inf' ))
      dbSendQuery(ec, " alter table base add index (setid) " )
   
      dbSendQuery(ec, paste( 'drop table if exists catches' ) )
      dbWriteTable(ec, 'catches', catches, overwite=T, row.names=F)
      dbSendQuery(ec, " alter table catches add index (setid2) " )
 
      dbSendQuery(ec, paste( 'drop table if exists tmp' ) )
      dbSendQuery(ec, paste( 
            'create table tmp', 
            'select * from base',
            'left join catches on base.setid=catches.setid2'
            ))
    
      dbSendQuery(ec, paste( 'drop table if exists base' ) )
      dbSendQuery(ec, paste( 'create table base select * from tmp' ))

      dbSendQuery(ec, paste( 'drop table if exists tmp' ) )
      dbSendQuery(ec, paste( 'drop table if exists catches' ) )
      
      set = NULL
      set = dbGetQuery( ec, statement = "select * from base" )
 
    }
    
    set = cleanup.set( set )  # local modifications and filters to the database

    return(set)
  }

      
