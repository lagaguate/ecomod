
# -------------------------------------
#  fecundity estimated from predicted mean fecundity (based upon allometric relationship)
#  and kriged estimated numerical abundance of females ... does not directly account for allometry

  fecundity.indirect = function( p, outdir ) {
    
    x = snowcrab.db(DS="det.initial")
    y = snowcrab.db(DS="set.merge.netmind")[, c("trip", "set", "yr")]
    x = merge(x, y, by= c("trip","set"), all.x=T, all.y=F, sort=F)

    primiparous = filter.class( x, "primiparous")
    multiparous = filter.class( x, "multiparous")

    # estimates of mean fecundity of females in a given year
    fem = data.frame( years= sort(unique( as.numeric(as.character(x$yr)) ) ))
    fem$primi.mean = tapply( x$fecundity[primiparous], INDEX=list( yr=x$yr[primiparous] ), FUN=mean, na.rm=T, simplify=T )
    fem$primi.sd = tapply( x$fecundity[primiparous], INDEX=list( yr=x$yr[primiparous] ), FUN=sd, na.rm=T, simplify=T )
    fem$primi.n  = tapply( x$fecundity[primiparous], INDEX=list( yr=x$yr[primiparous] ), FUN=function(d) {length( which(!is.na(d))) }, simplify=T )
    fem$multi.mean = tapply( x$fecundity[multiparous], INDEX=list( yr=x$yr[multiparous] ), FUN=mean, na.rm=T, simplify=T )
    fem$multi.sd = tapply( x$fecundity[multiparous], INDEX=list( yr=x$yr[multiparous] ), FUN=sd, na.rm=T, simplify=T )
    fem$multi.n  = tapply( x$fecundity[multiparous], INDEX=list( yr=x$yr[multiparous] ), FUN=function(d) {length( which(!is.na(d))) }, simplify=T )
  
    
    p$vars.to.model = c( "totno.female.primiparous", "totno.female.multiparous" )
    p$years.to.model = 1998:p$current.assessment.year
    p = make.list( list(y=p$years.to.model, v=p$vars.to.model ), Y=p )
    K = interpolation.db( DS="interpolation.simulation", p=p )
    
#    K = K[ -which( K$yr <= 1998 ), ]

    K$vars = as.character(K$vars)
    # K = K[ which(K$vars %in% vars ) ,]
  
# total abundance estimates
  
   a.sum = function(x, y) tapply( x, y, sum, na.rm=T )
   a.sum( K$total, list(K$yr, K$vars) )
 
   st = snowcrab.db("set.complete")  ### not used ???
   offsets = c(NA,NA)
    
   fem = merge( fem, K[ which( K$vars=="totno.female.primiparous") ,], by.x="years", by.y="yr", sort=T, all.x=T, all.y=F )
   fem$v = NULL
   fem = merge( fem, K[ which( K$vars=="totno.female.multiparous") ,], by.x="years", by.y="yr", sort=T, all.x=T, all.y=F, 
    suffixes=c(".primi", ".multi") )
   fem$v = NULL

   # fem[ which(is.na(fem)) ] = 0

   fem$total.egg.primi = fem$primi.mean * fem$total.primi     
   fem$total.egg.multi = fem$multi.mean * fem$total.multi    
   fem$total.egg.all   = fem$total.egg.primi + fem$total.egg.mult

    # propagate the errors from above operations
   fem$total.egg.primi.sd = fem$total.egg.primi * sqrt( (fem$primi.sd / fem$primi.mean)^2 + ( (0.2*fem$total.primi) /fem$total.primi)^2 )     
   fem$total.egg.multi.sd = fem$total.egg.multi * sqrt( (fem$multi.sd / fem$multi.mean)^2 + ( (0.2*fem$total.multi)/fem$total.multi)^2 )     
   fem$total.egg.all.sd   = sqrt(fem$total.egg.primi.sd ^2 + fem$total.egg.multi.sd^2)  

   yval =  log10(fem$total.egg.all)
   yval[ !is.finite(yval) ] = NA
   fem$upper = log10( fem$total.egg.all + 2 * fem$total.egg.all.sd )
   fem$lower = log10( fem$total.egg.all - 2 * fem$total.egg.all.sd )
   
   datarange = range( c(yval, fem$upper, fem$lower), na.rm=T)
   yrs = range( fem$years, na.rm=T )
   yrs = c(-0.5, 0.5) + yrs
  
   fn = file.path(  outdir, "fecundity" )
   Cairo( file=fn, type="pdf", bg="white", units="in", width=6, height=8 )

   plot( fem$years, yval, type="b", xlab="Year", ylab="Potential total egg production (log10)", xlim=yrs, ylim=datarange )
   arrows( x0=fem$years, y0=fem$upper, x1=fem$years, y1=fem$lower, angle=90, code=3, length=0.05  )
  
  dev.off()
   cmd( "convert   -trim -quality 9  -geometry 200% -frame 2% -mattecolor white -antialias ", paste(fn, "pdf", sep="."),  paste(fn, "png", sep=".") )
   table.view( fem)

   return(fem)

  }


