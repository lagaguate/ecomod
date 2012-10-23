  

  kriging.db = function( ip=NULL, DS, p=NULL, yrs, overwrite.reference=homedir, overwrite.threshold=NULL,  env.init=NULL ) {
    
    outdir = file.path( project.directory("snowcrab"), "R", "kriging" )
    dir.create(path=outdir, recursive=T, showWarnings=F)
  
  
    if ( DS %in% c( "UK.redo", "UK.point.PS", "UK.conditional.simulation.PS", "UK.conditional.simulation.K", "UK.conditional.simulation.K.complete" ) ) {
     

      loc.Pnt = file.path( project.directory("snowcrab"), "R", "kriging", "point" )
      loc.sol = file.path( project.directory("snowcrab"), "R", "kriging", "predictions" )
      loc.res = file.path( project.directory("snowcrab"), "R", "kriging", "results" )
     
      if (p$transgaussian.kriging) {
        loc.Pnt = file.path( loc.Pnt, "trans.gaussian" )
        loc.sol = file.path( loc.sol, "trans.gaussian" )
        loc.res = file.path( loc.res, "trans.gaussian" )
      } else { 
        loc.Pnt = file.path( loc.Pnt, "gaussian" )
        loc.sol = file.path( loc.sol, "gaussian" )
        loc.res = file.path( loc.res, "gaussian" )
      }

      dir.create(path=loc.Pnt, recursive=T, showWarnings=F)
      dir.create(path=loc.sol, recursive=T, showWarnings=F)
      dir.create(path=loc.res, recursive=T, showWarnings=F)

 
      if (DS=="UK.point.PS" ) {
        out = NULL
          v = p$v
          y = p$y
          r = p$r
          fn.Pnt = file.path( loc.Pnt, paste( "PS", v, y, r, "rdata", sep="." ) )
          if ( ! (file.exists( fn.Pnt)) ) return(NULL)
          load(fn.Pnt)
        return ( z )
      }
       
      if( DS=="UK.conditional.simulation.K.complete") {
          load( p$ofname )
          return( K ) 
      }

      if (!is.null(env.init)) for( i in env.init ) source (i)
      if (is.null(ip)) ip = 1:p$nruns

      if (DS %in% c("UK.conditional.simulation.K")  ) {
        out = NULL
        for ( iip in ip ) {
          y = p$runs[iip,"y"]
          r = p$runs[iip,"r"]
          v = p$runs[iip,"v"]
          fn.K = file.path( loc.res, paste( "K", v, y, r, "rdata", sep="." ) )
          if ( ! (file.exists( fn.K)) ) next()
          load(fn.K)
          
          if (is.null(K$kappa)) K$kappa=NA  ## tmp fix 
          if (is.null(K$datestamp)) K$datestamp=NA  ## tmp fix 

          out = rbind( out, K )
        }  
        K = out
        save( K, file=p$ofname, compress=T )  # glue all the data results together into one file
        return ( K )
      }
           

      if (DS=="UK.conditional.simulation.PS" ) {
        out = NULL
          v = p$v
          y = p$y
          r = p$r
          fn.PS = file.path( loc.sol, paste( "PS", v, y, r, "rdata", sep="." ) )
          if ( ! (file.exists( fn.PS)) ) return(NULL)
          load(fn.PS)
        return (PS)
      }
      
      # get data for the whole area
      all.set.years = c(1998:p$current.assessment.year)
      var.list= c( all.vars( p$kformula), "yr", "plon", "plat")
      K = NULL



      for ( iip in ip ) {

        y = p$runs[iip,"y"]
        r = p$runs[iip,"r"]
        v = p$runs[iip,"v"]

        if ( ! is.null( overwrite.threshold )  ) {
          fn.test= NULL
          if ( "point.kriging" %in% p$kriging.type ) fn.test = file.path( loc.Pnt, paste( "PS", v, y, r, "rdata", sep="." ) )
          if ( "block.conditional.sims" %in% p$kriging.type ) fn.test = file.path( loc.res, paste( "K", v, y, r, "rdata", sep="." ) )
          if ( overwrite.threshold > 0 ) {  # any value <= 0 will force an overwrite
            if ( file.exists( fn.test ) ) {
              load( fn.test) 
              current.time = as.chron(Sys.time(), out.format=c(dates="year-m-d", times="h:m:s") )
              file.time = string2chron( K$datestamp )
              if (( current.time - file.time ) < overwrite.threshold ) next()
            }
          } else {  # less than 0 -- only overwrite if no results found
            if ( file.exists( fn.test ) ) next()
          }
        }
  
        S  = snowcrab.db( DS="set.logbook", yrs=all.set.years,p=p ) 
      
        if (p$transgaussian.kriging) {
          # S = S[ which( S[,v] > 0) ,]
          S[,v] = variable.recode( S[,v], v, direction="forward", db="snowcrab" )
        }

        S = rename.df(S, v, "kv")
        S = S[, var.list ]

        # jitter is used to prevent singular solutions when stations are too close to each other
        S$plon = jitter(S$plon)
        S$plat = jitter(S$plat)  
        
#        # run a global variogram to obtain constants:: the whole region  (CFAALL and various year combinations are attempted)
#        # the returned semivariance are scaled to unit variance (of the input data)
        vgm.empirical.global = try (model.variogram.running (S, v, y, p, empirical.vgm.only=T), silent=T ) 
        if (is.null(vgm.empirical.global) | ( "try-error" %in% class(vgm.empirical.global) ) ) {
          print( paste( "Empirical Variogram problem with:", v, y, r ) ) 
          print (vgm.empirical.global)
          #      next()  # no data for emprical variogram
        }
        
        iPS = iRegion.PS =iRegion.Sy = z = vgm.e = vgm.m = NULL
 
        # 1. get data for the year in question 
        Sy = S[ which(S$yr==y) ,]
        PS = snowcrab.habitat.db( DS="PS", yrs=y, p=p ) 
        
       
        # 2. habitat area for snow crab
        iHabitat = which( PS$habitat > 0.5 & (PS$habitat.se < PS$habitat ) )  # from a scatterplot, those se > 0.4 seem highly extreme 
        # iHabitat = which( PS$habitat > 0.5 & PS$habitat.se < 0.4 )  # from a scatterplot, those se > 0.4 seem highly extreme 
        # iHabitat = which( ( PS$habitat - PS$habitat.se*2 ) > 0.5 ) 

        # 2B ..  # trim PS to make the following less memory intensive as other vars are no longer needed
        PS = PS[ , setdiff(var.list, "kv") ]
        
        # 3. that which is in the region of interest
        iRegion.Sy = filter.region.polygon(x=Sy[ , c("plon", "plat")], region=r, planar=T)
        iRegion.PS = filter.region.polygon(x=PS[ , c("plon", "plat")], region=r, planar=T)
        
        totalsurfacearea = length( iHabitat ) * (p$pres*p$pres)  # only habitat space being considered

        # always add areas within a certain distance of a tow, even if they are not in "good habitat"  --- for plotting only
        # keeping all points within X km of a survey station
        distances = rdist( PS[,c("plon", "plat")], Sy[ iRegion.Sy, c("plon", "plat") ])  
        dd = distances
        dd[ which(dd < p$threshold.distance) ] =  NA   # mark those within x km of a survey station
        survey.buffer = sort( which( !is.finite( rowSums(dd ) ) ) ) 
        
        # plotting surface
        iPlot = union( iHabitat, survey.buffer)
        iPlot = intersect( iRegion.PS, iPlot ) 
       
        # prediction surface
        # 5. prediction surface and set-level data for abundance estimation in region of interest
        # including only areas within a given distance from survey stations and east of the SW point of NS
        # remove all areas that are too far from a station p$kriging.extrapolation.limit
        iPS = union( iHabitat, survey.buffer)
        iPS = intersect( iRegion.PS, iPS ) 
          # before continuing, extract SA estimates 
          surfacearea = length(iPS) * (p$pres*p$pres)
        ee = distances
        ee[ which(ee < p$kriging.extrapolation.limit) ] =  NA   # mark those within x km of a survey station
        extrap.buffer = sort( which( !is.finite( rowSums(ee ) ) ) ) 
        iEastof250 = which( PS$plon > 250 )
        iPS = intersect( iPS, iEastof250 )
        iPS = intersect( iPS, extrap.buffer)  

        rm (survey.buffer, dd , distances, iRegion.PS , iHabitat,extrap.buffer, ee  ); gc()

        if ( length(iPS) < 30 ) next() # no available habitat space of significance

        # add a buffer area around the region for input data to reduce edge effects
        i.input.data = iRegion.Sy 
        if ( !is.null( p$kriging.distance.buffer ) ) {
          Sdistances =  rdist( Sy[ ,c("plon", "plat")], Sy[ iRegion.Sy, c("plon", "plat") ])
          Sdistances[ which( Sdistances < p$kriging.distance.buffer ) ] =  NA
          S.to.keep = sort( which( !is.finite( rowSums( Sdistances) ) ) )
          i.input.data = union( i.input.data, S.to.keep )
          rm (Sdistances, S.to.keep)
        } 
        if (length( i.input.data) < 10 ) next() #no data
        rm( iRegion.Sy ); gc()

        # begin variogram modelling
        if (p$use.local.variograms ) {
          iRegion.S = filter.region.polygon(x=S[, c("plon", "plat")], region=r, planar=T)
          vgm.e0 = NULL
          vgm.e0 = try (model.variogram.running (S=S[ iRegion.S, ], v=v, y=y, p=p, empirical.vgm.only=T), silent=T ) 
          if (is.null(vgm.e0) | ( "try-error" %in% class(vgm.e0) ) ) {
            vgm.e0 = vgm.empirical.global
            print( paste( "Empirical Variogram problem with:", v, y, r ) ) 
            print (vgm.e0)
            print( "Using global Variogram instead due to small sample size or unstable variogram" )
          }
        } else {
          vgm.e0 = vgm.empirical.global
        }
        
        vgm.test = try( gstat.model.variogram ( vgm.e0, vp=get.variogram.params(v, y) ), silent=T)  # the get.vario .. is a blacklist of unusable variables/ years
        if ( "try-error" %in% class(vgm.test) ) {
          print( paste( "Empirical Variogram problem with:", v, y, r ) ) 
          print (vgm.e0)
          next()  # no data for emprical variogram
        }
        # scale the empirical variogram to the variance in the data in the sub-area and then obtain the variogram model
        var.e = var( Sy[i.input.data,"kv"], na.rm=T )
        vgm.e = vgm.e0  # vgm.e0 is the global variogram for the whole region/year scaled to unit variance
        vgm.e$gamma = vgm.e$gamma * var.e  # rescale variance to that of the specific sub-area/year of interest
        vgm.m = try( gstat.model.variogram ( vgm.e, vp=get.variogram.params(v, y) ), silent=T)  # the get.vario .. is a blacklist of unusable variables/ years
          
        if ( is.null(vgm.m) | ( "try-error" %in% class(vgm.m) )  ) {
          print( paste( "Modelled variogram problem with:", v, y, r ) ) 
          print( vgm.e )
          print( vgm.m )
          next()  # no viable solutions
        }
        
        print( paste( "Kriging with:", v, y, r ) ) 

        vario.sse = ifelse(is.null(attr(vgm.m, "SSErr")), NA, attr(vgm.m, "SSErr") )
        vario.model = vgm.m$model[2]
        psill = vgm.m$psill[2]
        nugget = vgm.m$psill[1]
        range = vgm.m$range[2]
        kappa = ifelse ( vario.model=="Mat", vgm.m$kappa[2], NA )
 
        g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=Sy[i.input.data,], nmin=1, nmax=p$knmax )
        z = K0 =NULL

        if ( "point.kriging" %in% p$kriging.type ) {
          fn.Pnt = file.path( loc.Pnt, paste( "PS", v, y, r, "rdata", sep="." ) )
          print (fn.Pnt )
         # point kriging .. used for plotting/mapping
          z = try( predict(object=g, newdata=PS[iPlot,], debug=-1), silent=T)
          if ( "try-error" %in% class(z) )  {  # increase the number of stations
             g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=Sy[i.input.data,], nmin=1, nmax=p$knmax+20  )  
             z = try( predict(object=g, newdata=PS[iPlot,], debug=-1), silent=T)
          }

          if ( "try-error" %in% class(z) )  {  # increase the number of stations
             g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=Sy[i.input.data,]  )  
             z = try( predict(object=g, newdata=PS[iPlot,], debug=-1), silent=T)
          }
            if ( "try-error" %in% class(z) ) next() 
      
          if (p$transgaussian.kriging)  {
            z[,3] = variable.recode( z[,3], v, direction="backward", db="snowcrab" )
            z[,4] = variable.recode( sqrt( z[,3]), v, direction="backward", db="snowcrab" )
            z[,4] = z[,4] ^ 2
          }
          er = empirical.ranges( db="snowcrab", v )
          z[ which(z[,3]< er[1]) , 3 ] = er[1]  # assume too small to detect .. 0
          z[ which(z[,3]> er[2]) , 3 ] = er[2]  # upper bound
          save (z, file=fn.Pnt, compress=T)
          print( "... Completed successfully, Now plotting ..." ) 
          map.krige.lattice( M=list(nruns=1, runs=p$runs[iip,], transgaussian.kriging=p$transgaussian.kriging ) )
        }

        if ( "block" %in% p$kriging.type )  {  # not working as expected ... 
#         try to use  SpatialPixelsDataFrame
          # block kriging .. old method
#          PS$plon0 = PS$plon
#          PS$plat0 = PS$plat
          centroid = as.data.frame( t( apply( PS, 2, FUN=function(x){ mean (as.numeric(x), na.rm=T)} )))
#          PS$plon = PS$plon0 - centroid$plon
#          PS$plat = PS$plat0 - centroid$plat
          centroid$plon=0
          centroid$plat=0
#          z = try( predict(object=g,  newdata=centroid, block=PS[iPS,c("plon", "plat")], debug=-1), silent=T)
          z = try( predict(object=g,  newdata=centroid, block=PS[iPS,c("plon", "plat")], debug=-1), silent=T)
          if ( "try-error" %in% class(z) )  next() 
          datacols = c(3:ncol(z))
          if (p$transgaussian.kriging)  z[,datacols] = variable.recode( z[,datacols], v, direction="backward", db="snowcrab" )
          z.sum = z[,3] * surfacearea
          z.sum.sd = sd( z.sum )
          z.sum.mean = mean ( z.sum )
          ci = quantile( z.sum, probs=c(0.025, 0.5, 0.975), na.rm=T,names=F )
          lbound = ci[1]
          ubound = ci[3]
          median = ci[2]
          K0 = data.frame( yr=y, vars=v, region=r, var=var.e, total=z.sum.mean, median, lbound, ubound,
            totalsurfacearea, surfacearea, vario.model, vario.sse, psill, nugget, range, kappa, datestamp=as.character(Sys.time() ) )
        } 
        
        if (  "block.conditional.sims"  %in% p$kriging.type  ) {
          # block kriging with Gaussian conditional simulation ("trans-Guassian" sensu Cressie)
          # via direct summation of back-transformed data 
          
          fn.PS = file.path( loc.sol, paste( "PS", v, y, r, "rdata", sep="." ) )
          fn.K = file.path( loc.res, paste( "K", v, y, r, "rdata", sep="." ) )
    
          print(fn.K)

          z = try( predict(object=g, newdata=PS[iPS,], block=c(p$pres,p$pres), nsim=p$n.conditional.sims, debug=-1), silent=T)
          if ( "try-error" %in% class(z) )  { # increase the number of stations
             g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=Sy[i.input.data,], nmin=1, nmax=p$knmax+20 )  
             z = try( predict(object=g, newdata=PS[iPS,], block=c(p$pres,p$pres), nsim=p$n.conditional.sims, debug=-1), silent=T)
          }
          if ( "try-error" %in% class(z) )  { # increase the number of stations
             g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=Sy[i.input.data,], nmin=1, nmax=p$knmax+40 )  
             z = try( predict(object=g, newdata=PS[iPS,], block=c(p$pres,p$pres), nsim=p$n.conditional.sims, debug=-1), silent=T)
          }
            
          if ( "try-error" %in% class(z) )  { # increase the number of stations
             g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=Sy[i.input.data,] )  
             z = try( predict(object=g, newdata=PS[iPS,], block=c(p$pres,p$pres), nsim=p$n.conditional.sims, debug=-1), silent=T)
          }
           
          if ( "try-error" %in% class(z) ) next() 
          
          z = as.matrix( z[ , 3:ncol(z) ] )
    
          # zcd = apply( z,1,mean,na.rm=T ) / apply( z, 1, sd, na.rm=T )
          # ibad = which( zcd > 1 ) # large coefficients of variation are excluded

          if (p$transgaussian.kriging) z = variable.recode( z, v, direction="backward", db="snowcrab" )
          er = empirical.ranges( db="snowcrab", v )
          
          # z[ ibad,] = 0 # drop them from further consideration
          
          # remove extremes
          ii = which(z < er[1])
          if (length (ii) > 0 )  z[ ii ] = 0
          
          jj = which( z > er[2] )
          if (length(jj) > 0 )   z[ jj ] = er[2]
           
          # remove estimates that are lower than the quantile at which "habitat" is considered viable
          # this is because these low value estimates are highly uncertain but can amount to a large  surface area and so have a large influence
          #ik = which( z < quantile( z[which(z>0)], prob=p$habitat.threshold.quantile ) )  
          #if (length (ik) > 0 )  z[ ik ] = 0


          PS = PS[iPS, c("plon", "plat")]
          PS$pred.mean = apply( z, 1, mean, na.rm=T )
          PS$pred.sd = apply( z, 1, sd, na.rm=T )
          save(PS, file=fn.PS, compress=T)
          
          rm(ii, jj, PS); gc()
          
          z.sum = apply( z, 2, sum, na.rm=T )
          z.sum.sd = sd( z.sum )
          z.sum.mean = mean ( z.sum )
          ci = quantile( z.sum, probs=c(0.025, 0.5, 0.975), na.rm=T,names=F )
          lbound = ci[1]
          ubound = ci[3]
          median = ci[2]
          K = data.frame( yr=y, vars=v, region=r, var=var.e, total=z.sum.mean, median, lbound, ubound,
            totalsurfacearea, surfacearea, vario.model, vario.sse, psill, nugget, range, kappa, datestamp=as.character( Sys.time() ) )
  
          save(K, file=fn.K, compress=T)
          print (K)
          print( "... Completed successfully" )  
        }
     
      } # runs (ip)
    } # end UK 


  }



  # ----------------------------------

krige.map = function( p=NULL, init.files=NULL ) {
  if (!p$do.parallel) {
    krige.map.core( p=p, init.files=init.files )
  } else  {
     
      pp = prep.parallel.run( p$clusters, p$nruns )
    clusterApplyLB( pp$cl, pp$ssplt, krige.map.core, p=p, init.files=init.files )
    stopCluster(pp$cl)
  }
}

# --------------------------------------------------------------------


  subselect.xy2grid = function(xy=NULL, area="cfaall", resolution=NULL, DS="file", loc="grids", fname="cfa.grid" ) {
    fname = file.path(loc, paste(fname, area, "rdata", sep="."))
    if (DS=="redo") {
      inside = filter.region.polygon(xy, area)
      xy$areafilter = NA
      xy$areafilter[inside] = 1
      grid = xyz2grid ( xyz=xy, xx=lons, yy=lats)

      loc = dirname( fname )
      dir.create( path=loc, recursive=T, showWarnings=F )
      save( grid, file=file.path(loc,fname), compress=T)

    }
    if (DS=="file") load (fname)
    return(grid)
  }
   # -------------------------

  # -----------------------------------------------------------


   get.snowcrab.habitat.sa.ts = function (years, v, p) {

      for (y in years) {

        zrange = get.zrange (v)
        load (file.path( project.directory("snowcrab"), "R", "bathy.gridded.rdata"))
        bathy[bathy>=zrange[1] | bathy<=zrange[2]] = NA

        # construct prediction locations
        prediction.locations = bathy * tgrid.lt.500m * mask
        image.plot(prediction.locations, lons, lats)

        prediction.locations.xyz = lonlat2planar( grid2xyz(prediction.locations, lons, lats) , proj.type=p$internal.projection )
        plons = trunc(prediction.locations.xyz$plon/pres) *pres
        plats = trunc(prediction.locations.xyz$plat/pres) *pres
        out = unique(data.frame(cbind(plon=plons, plat=plats)))
        total.surfacearea.trawled = dim(out)[1]

        rm (bathy, prediction.locations.xyz, prediction.locations, plons, plats)

      }
    }


  # -----------------------------------------------------------

  gstat.model.variogram = function(vgm.e, vp) {
    vgm.m=NULL
    all= NULL
    if (vp$skip) return(NULL)
    if (vp$fit) {
      vmodels = c( "Sph", "Exp", "Cir", "Pen" )
    }

    # initial start -- based upon empirical observations of stable variograms
    vgm.e$id = "var1"
    vp$psill = max(vgm.e$gamma) * 0.2
    vp$nugget = max(vgm.e$gamma) * 0.3
    vp$range = max(vgm.e$dist) *0.25

    vp0 = vp

    # find estimates of nugget and psill
    all = find.best.variogram.model( vmodels, vp, vgm.e )
    all = all[ which(all$range < (max(vgm.e$dist) ) ) ,] # likely solutions
    all = all[ which(all$range > (min(vgm.e$dist) ) ) ,] # likely solutions
    #      all = all[ which(all$psill < (max(vgm.e$gamma) ) ) ,] # likely solutions
    
    use.default = F 
    if (nrow(all) > 0 ) {
      good = all[ which.min(all$sse), ]
      vgm.m = vgm( psill=good$psill, model=good$model, range=good$range, nugget=good$nugget, kappa=good$kappa )
      vgm.m = fit.variogram(vgm.e, model=vgm.m)
      if ( vgm.m$range[2] > (max(vgm.e$dist) ) )  use.default=T
      if ( vgm.m$range[2] < (min(vgm.e$dist) ) )  use.default=T
    }
    
    if (is.null( vgm.m) | use.default ) { # still null .. use a simple default Spherical with conservative settings
      vgm.m = vgm( psill=vp0$psill, model="Sph", range=vp0$range, nugget=vp0$nugget )
      # vgm.m = fit.variogram(vgm.e, model=vgm.m)
    }
    return (vgm.m)
  }

  # --------------------------------------------------------------
    find.best.variogram.model = function (vmodels, vp, vgm.e, fix.nugget=F) {
      r = NULL
      for (model in vmodels) {
        vm0 = NULL
        if (substring(model,1,3) == "Mat") {
          vp$kappa = as.numeric(substring(model, 4, nchar(model)))
          vp$var.mod = "Mat"
          vm0 = vgm( psill=vp$psill, model=vp$var.mod, range=vp$range, nugget=vp$nugget, kappa=vp$kappa )
        } else {
          vp$kappa = NA
          vp$var.mod = model
          vm0 = vgm( psill=vp$psill, model=vp$var.mod, range=vp$range, nugget=vp$nugget )
        }
        vmf = NULL
        if (! fix.nugget) {
          vmf = try ( fit.variogram(object=vgm.e, model=vm0), silent=T )
        } else  {
          vmf = try ( fit.variogram(object=vgm.e, model=vm0, fit.sill=F), silent=T )
        }

# fit.method: default fitting method, used by gstat. The default method uses
# weights $N_h/h^2$ with $N_h$ the number of point pairs and
# $h$ the distance. This criterion is not supported by theory,
# but by practice.  For other values of 'fit.method', see table
# 4.2 in the gstat manual.
# fit.method=5 is REML .. slower but more accurate

        if (!( "try-error" %in% class(vmf) ) )   {
            r = rbind(r, data.frame(cbind(
                    sse    = attr(vmf,"SSErr"),
                    model  = vp$var.mod,
                    range  = vmf$range[2],
                    psill  = vmf$psill[2],
                    nugget = vmf$psill[1],
                    kappa  = vp$kappa )))
       }
     }

      if (!is.null(r)) {
        r = factor2number(r, c("sse", "range", "psill", "nugget", "kappa"))
        r = factor2character(r, "model")
      }

      return (r)
    }

   # ----------------------------------------------------------------------

  get.variogram.params = function(v, y) {
    vm = data.frame( skip=F, fit=T )  # default
    #if (    (v=="R0.mass" & y==1997)
    #     |  (v=="R0.no" & y==1997)
    #     |  (v=="totmass.male.com.CC1to2" & y==2001)
    #   )
    #   {
#         vm = data.frame( skip=T,  fit=F )
    # }
    return (vm)
  }

  # -----------------

 
  # ------------------

  regrid.lonlat = function (old, res, vr.to.sum=NULL, vr.to.cp=NULL) {
    old$gridid = paste(old$plon%/%res*res, old$plat%/%res*res, sep="." )
    new=data.frame( gridid = I(sort(unique(old$gridid)) ))
    for (vr in vr.to.sum) {
      l = aggregate(old[,vr], by=list(old$gridid), FUN=sum, na.rm=T)
      names(l) = c("gridid", vr)
      l$gridid = as.character(l$gridid)
      new = merge(x=new, y=l, by="gridid", all.x=T, all.y=F, sort=F)
    }
    if (!is.null(vr.to.cp) ) {
    for (vr in vr.to.cp) {
      l = aggregate(old[,vr], by=list(old$gridid), FUN=function(x) sort(unique(x))[1] )
      names(l) = c("gridid", vr)
      l$gridid = as.character(l$gridid)
      new = merge(x=new, y=l, by="gridid", all.x=T, all.y=F, sort=F)
    }}
    new$gridid = as.character( new$gridid )
    
    return( new )
  }

  # ------------------

  # ----------------------------------
  

  # ----------------------------------

  model.variogram.running = function( S, v, y, p, empirical.vgm.only=F ) {

    # construct empirical (residual) variogram and model
    yrs =  y + c(-3:3 )
    weights = c( 1/4, 1/2, 3/4, 1, 3/4, 1/2, 1/4 )  # linear decay of weights

    datayrs = unique( S$yr )
    # running variogram 
    out= NULL
    vgm.e = vgm.e0 = buf = np = v.scale = vgm = wts = NULL
    lvars =  c( all.vars( p$kformula) )

    for ( iy in 1:length(yrs) ) { # obtain historical variograms
      if ( yrs[iy] %in% datayrs ) {
        q = which(S$yr==yrs[iy] )
        if (length(q) > 30) {# final check
  #        q = block.xyz2xyz( xyz=q, params=p, engine="R2", variables=lvars )
  #        q$t[!is.finite(q$t)] = mean(q$t, na.rm=T, trim=0.1) # temporaily add  means to force a solution
  #        q$z[!is.finite(q$z)] = mean(q$z, na.rm=T, trim=0.1)
          vgm = try( variogram(id="kv", p$kformula, loc=p$klocs, data=S[q,], boundaries=p$vgm.dist, cutoff=max(p$vgm.dist) + 20, cressie=T ), silent=T)
          if  (! ( "try-error" %in% class(vgm) )  ) { 
            # vgm$gamma = vgm$gamma  # normalised to unit variance for each year
            buf = cbind(buf, vgm$gamma )
            np  = cbind(np, vgm$np)
            wts = c(wts, weights[iy] )
          }
        }
      }
    }
   
    if (is.null(buf)) return(NULL)
    vgm.e = variogram(id="kv", p$kformula, loc=p$klocs, data=S, boundaries=p$vgm.dist, cutoff=max(p$vgm.dist) + 20, cressie=T  ) # obtain template
    vgm.e$gamma = apply( buf, 1, weighted.mean, w=wts, na.rm=T)  # still normalised to unit variance
    vgm.e$np = round( apply( np, 1,  weighted.mean, w=wts, na.rm=T) ) 
    vgm.e$id = "kv"
    
    if (empirical.vgm.only) return (vgm.e)

    vgm.m = try( gstat.model.variogram ( vgm.e, vp=get.variogram.params(v, y) ), silent=T )
    if ( is.null(vgm.m) | ( "try-error" %in% class(vgm.m) ) )  return (NULL )
    

    out = list(vgm.e=vgm.e, vgm.m=vgm.m)
    
    print(vgm.e)
    print(vgm.m)

    if (p$plot.variogram) {
      # plot finalised variograms
      loc = file.path(  project.directory("snowcrab"), "R", "variograms" )
      fname = file.path(loc, paste(v, y,"png", sep="."))
      dir.create(path=loc, recursive=T, showWarnings=F)
      png(filename=fname)
      plot( x=vgm.e$dist, y=vgm.e$gamma, ylab="Semi-variance", xlab="Distance (km)", pch=20 )
      xaxis = range(p$vgm.dist)
      yaxis = range(vgm.e$gamma)
      textout = paste(
          " Year    = ", y, " ~ mean: ", vyrs.try[vt],
        "\n Kriging = ", deparse(p$kformula),
        "\n Model  = ", vgm.m$model[2],
        "\n Psill     = ", signif(vgm.m$psill[2],3),
        "\n Nugget = ", signif(vgm.m$psill[1],3),
        "\n Range  = ", signif(vgm.m$range[2],3),
        "\n Kappa  = ", signif(vgm.m$kappa[2],3), sep="" )
      lines( variogram.line(vgm.m, maxdist=xaxis[2] ), lty="dashed" )
      text ( x=xaxis[2] *0.5, y= yaxis[2] * 0.6, textout, adj=0, cex=0.8)
      dev.off()
    }

    return( out )
  }


  # ------------------------------------

# ----------------------------
# ----------------------------
# ----------------------------
# The following are parallelized versions of the main routines 
# --------------------------
# --------------------------
# --------------------------


# ------------------------------
# -----------------------

# ----------------------------------------
krige.map.core = function( ip=NULL, p=NULL, init.files=NULL  ) {
   
  for (i in init.files) source( i )
  if ( is.null(id)) ip = c(1: p$nruns ) 

  for (iip in ip) {
        y = p$runs[iip,"y"]
        r = p$runs[iip,"r"]
        v = p$runs[iip,"v"]

    k.pname = paste(v, "pred", sep=".")
    k.vname = paste(v, "var",  sep=".")

#  these results have already been interpolated
#  .. reduce the degree of interpolation in the mapping of the results

    p$tension = "-T1"  # 0.35+ for steep; 0.25 for smooth
    p$maskres = "-S1k"  # effectively turns off interpolation
    p$interpres = "-S1k"

    S = get.PS.S.gridded (p, y, v)$S
    mest = variable.recode( S[,"kv"], v, direction="forward" )  # used for generating a colour scale that matches the whole time span
    rm (S); gc()
    
    bk0 = krigng.db( DS="UK.point.PS", p=list(v=v, y=y, r=r) )

    if (is.na(bk0) ) next ()
    bk = bk0$bk
    rm (bk0); gc()

    mdata = planar2lonlat( bk, proj.type=p$internal.projection ) # for mapping
#    mdata[,k.pname] = variable.recode( mdata[,k.pname], v, direction="backward")
#    mdata[,k.vname] = variable.recode( sqrt(mdata[,k.vname]), v, direction="backward")
    mdata[,k.vname] = sqrt(mdata[,k.vname])

    if (!file.exists(p$basemap) )  gmt.basemap(p)

# map means
    p$outdir = file.path( project.directory("snowcrab"), "R", "predictions", "kriged.estimates")
    dir.create (path=p$outdir, recursive=T, showWarnings=F)
    p$outfile.basename = file.path(p$outdir, paste(v, y, sep="."))
    p = gmt.define.colours (p, v)
    p = gmt.colourscale(p, mest, v, NSTD=3 ) # NSTD is no of stdev .. uniform colour scale
    gmt.map( p, mdata[, c("lon", "lat", k.pname)], y, v , conversions="ps2png" )

# map variances
    p$outdir = file.path( project.directory("snowcrab"), "R", "predictions", "kriged.variances")
    dir.create (path=p$outdir, recursive=T, showWarnings=F)
    p$outfile.basename = file.path(p$outdir, paste(v, y, sep="."))
    p = gmt.define.colours (p, v)
    p = gmt.colourscale(p, mdata[, k.vname], v, NSTD=3 ) # no need to force uniform scale as this is a diagnostic plot
    gmt.map( p, mdata[, c("lon", "lat", k.vname)], y, v, conversions="ps2png" )
   
  }
  return("Complete map")
}



# ----------------------------------------
  map.krige.lattice = function( ip=NULL, M=NULL, env.init=NULL, log.transf=T ) {

    loc = file.path( project.directory("snowcrab"), "R", "kriging", "maps" )

       if (M$transgaussian.kriging) {
         loc = file.path( loc, "trans.gaussian" )
       } else { 
         loc = file.path( loc, "gaussian" )
       }

    dir.create(path=loc, recursive=T, showWarnings=F)

    # ip is the first parameter passed in the parallel mode
    if (!is.null(env.init)) for( i in env.init ) source (i)
    if (is.null(ip)) ip = 1:M$nruns

    for (iip in ip) {
        y = M$runs[iip,"y"]
        r = M$runs[iip,"r"]
        v = M$runs[iip,"v"]
      
      k.pname = paste(v, "pred", sep=".")
      k.vname = paste(v, "var",  sep=".")

      PS = kriging.db( DS="UK.point.PS", p=list(v=v, y=y, r=r, transgaussian.kriging=M$transgaussian.kriging) )
      if (is.null(PS)) next()

      xx = snowcrab.db("set.complete")[, v] 
      er = range( xx, na.rm=T )

      if (log.transf) {
        er = range( xx[ which(xx >0)])
        er = log10(er)
        PS[, k.pname] = log10( PS[,k.pname] ) 
        PS[ which(PS[, k.pname] < er[1]) , k.pname] = er[1]
        PS[, k.vname] = log10( sqrt(PS[,k.vname])  )
      }

      # mean estimate
      datacols = c("plon", "plat", k.pname)
      datarange = seq( er[1], er[2], length.out=150)
      cols = color.code( "seis",datarange )
      outfn = paste( "prediction.mean", v, y, r, sep=".")
      annot = paste( v, y )
      map( xyz=PS[,datacols], cfa.regions=T, depthcontours=T, pts=NULL, annot=annot, 
          annot.cex=M$annot.cex, corners=planar.corners , 
        fn=outfn, loc=loc, at=datarange , col.regions=cols )
  
      # SD estimate
      datacols = c("plon", "plat", k.vname)
      datarange = seq( er[1]/2, er[2]*2, length.out=150)
      cols = color.code( "seis",datarange )
      outfn = paste( "prediction.sd", v, y, r, sep=".")
      annot = paste( v, y)
      map( xyz=PS[,datacols], cfa.regions=T, depthcontours=T, pts=NULL, annot=annot,
          annot.cex=M$annot.cex, corners=planar.corners ,
        fn=outfn, loc=loc, at=datarange , col.regions=cols )

    }
    return ("Complete")
  }


  # -----------------------

  prediction.surface = function( p, DS="annual", method="direct" , add.set.locations=F, vclass="R0.mass" ) {
    # prepare surface area and temperature estimates for plots
    
    if (DS %in% c("annual", "annual.redo" ) ) {

      fn = file.path( project.directory("snowcrab"), "R", "PS.habitat.ts.rdata" )    
      
      if (DS=="annual") {
        load(fn)
        return( PS.hab )
      }
      
      out = NULL
      
      # yrs =  p$yearswithTdata --- no longer possible as biological data also enter into prediction surface
      yrs = 1970:p$current.assessment.year

      for( y in yrs ){
        print(y)
        PS = habitat.db ( DS="complete", year=y, p=p )
        PS = fishing.area.designations( PS, type="planar")
        PS$cfa.factor = NULL
        pH = snowcrab.habitat.db ( DS="habitat", yrs=y, model.type="gam.full", vclass="R0.mass" )
        PS$habitat = pH$fit
        PS$habitat.se = pH$se.fit
        rm (pH ) ; gc()

        ips = NULL
        ips = filter.prediction.locations( DS="default", PS=PS )
        ips = unique( c( 
          ips, 
          filter.prediction.locations( DS="limit.to.near.survey.stations", PS=PS, y=y, p=p ) ) 
        ) # add areas near survey locations even if beyond the range of habitat

        PS = PS[ips, ]

        sa.habitat.total = length(ips )
        temp = tapply( PS$t, INDEX=list(PS$cfa, PS$yr ), FUN=mean, na.rm=T )
        temp.sd = tapply( PS$t, INDEX=list(PS$cfa, PS$yr ), FUN=sd, na.rm=T )
         sa = tapply( PS$t, INDEX=list(PS$cfa, PS$yr ), FUN=length )

        dat = as.data.frame.matrix( cbind( sa, temp, temp.sd) )
        names(dat) = c("sa", "t", "t.sd" )
        dat$cfa = rownames(dat)
       
        all = data.frame( cbind( sa=sa.habitat.total, t=mean(PS$t,na.rm=T), t.sd=sd(PS$t,na.rm=T)) ) 
        all$cfa = "cfaall"

        dat = rbind( dat, all)
        dat$yr = y
        dat$sa = dat$sa * (p$pres*p$pres)

        out = rbind( out, dat )
      }
      PS.hab = out
      save( PS.hab, file=fn, compress=T )

      return  ( PS.hab )
    }
  }




  model.habitat = function( model.type="gam.full", p=NULL, hvar="R0.mass", plotdata=T) {

    outdir = file.path( project.directory("snowcrab"), "R", "habitat", "models" )
    dir.create(path=outdir, recursive=T, showWarnings=F)

    qq =  grep( "redo", model.type)
    if ( length(qq) != 0  ) {
      set = snowcrab.db( DS="set.logbook", yrs=p$years.to.model, p=p )
      q = quantile( set[which(set[,hvar]>0), hvar], p$habitat.threshold.quantile ) 
      set$Y = 0 
      set$Y[ which( set[,hvar] > q ) ] = 1  
    }
        
    if ( model.type %in% c("gam.full", "gam.full.redo") ) {
      
			fn = file.path( outdir, paste( "habitat.gam.full", hvar, "rdata", set="." ) )

      if ( model.type == "gam.full" ) {
        load(fn)
        return( Q)
      }
      require(mgcv)
      require(arm)

#      .M.gam = formula( Y ~  s( yr ) + s( tmean ) + s( I(t-tmean) ) + s( tamp) + s( wmin ) + s( plon, plat) + s( z) 
#        + s( substrate.mean) + s( ddZ) +s( dZ)  ) 
      .M.gam = formula( Y ~  s( yr ) + s( t )  + s( tamp.annual) + s( wmin.annual ) + s( plon, plat) + s( z) 
        + s( substrate.mean) + s( ddZ) +s( dZ)  ) 
      Q = gam( .M.gam, data=set, na.action="na.pass", family=binomial() )
      AIC (Q) # = 3367.65
      summary(Q)
      # P = predict( Q, set, type="response")
      # cor(P,set$Y)^2 # = 0.41
      save( Q, file=fn, compress=T )
      if (plotdata) plot(Q, all.terms=T, rug=T, jit=T, seWithMean=T, trans=invlogit, pers=T , scale=0)
      return ( "Complete" )
    }
     


    if ( model.type %in% c("glm.splines.redo", "glm.splines") ) {
      fn = file.path( outdir, "habitat.gam.full.rdata" )
      if ( model.type == "glm.splines" ) {
        load(fn)
        return( Q)
      }

      # interaction terms were significant but were heaviliy influenced by extremes in data .. they were dropped
      .M.splines = formula( Y ~  bs(yr, df=4)+  tmean + I(t-tmean) + bs(tamp, df=3) +  bs(wmin, df=3) + bs( plon, df=3)* bs(plat,df=4)+  bs(z, df=4) + bs(substrate.mean, df=3) + bs(dZ, df=3)+  bs(ddZ, df=3)
        + bs(dZ,df=3) 
      ) 
      Q = glm( .M.splines, data=set, na.action="na.omit", family=binomial() )
      AIC (Q) # 3573.2 
      summary(Q)
      # p = predict( Q, set, type="response")
      cor(p,set$Y)^2 #
      if (plotdata) plot(Q, all.terms=T, rug=T, jit=T, seWithMean=T, trans=invlogit, pers=T )
      save( Q, file=fn, compress=T )
      return ( "Complete" )
    }

    if (model.type == "gamm.redo" ) {
          # GAMM
          ii = sample(1:nrow(set), 200 )
          set$id = factor( paste( set$plon, set$plat, sep="~") )
          set = set[, c("id", "plon", "plat", "Y", "yr.factor", "t", "z", "substrate.mean", "ddZ", "dZ") ] 
          .M.gamm = formula( Y ~ yr.factor + s(t) + s(z, substrate.mean) + s(ddZ, dZ) - 1 ) 
          # Q = gamm( .M.gam, random=list(yr.factor=~1), data=set, na.action="na.omit", family=binomial() )
          # deviance = 3014.19       2721  df; ~ 36% of deviance explained               
          Q = gamm( .M.gamm, random=list(yr.factor=~1+id), data=set, na.action="na.omit", family=binomial(), subset=ii)
          save( Q, file=file.path( outdir, "q.gamm.rdata" ), compress=T )
    }

    if (model.type =="gamm.correl" ) {
          # GAMM  with correlation
          set$plon = jitter(set$plon, amount=1)
          set$plat = jitter(set$plat, amount=1)
          # ~ 24 hr
          Q = gamm(  Y ~ s(yr) + s(z) + s(substrate.mean) + s(dZ) + s(ddZ) + s( tamp.annual) + s( wmin.annual )+ s( substrate.mean) , correlation=corGaus(form=~plon+plat),
          data=set, na.action="na.omit", family=binomial())
          # r = predict.gam( q.gamm.cor$gam, set, type="response")
          # cor(r,set$Y)^2 =  0.182554
          save( Q, file=file.path( outdir, "q.gamm.cor.rdata" ), compress=T )
    }

    if (model.type =="gamm.correl" ) {
          # ~ 68 hrs
          Q = gamm(  Y ~ yr.factor + s(t) + s(plon, plat) + s(z, substrate.mean) + s(dZ, ddZ), 
            correlation=corGaus(form=~plon+plat),
            data=set, na.action="na.omit", family=binomial())
            r2 = predict.gam( q.gamm.cor2$gam, set, type="response")
            cor(r2,set$Y)^2 
          # = 0.377711
            save( Q, file=file.path( outdir, "q.gamm.cor2.rdata" ), compress=T )
    }
           
  }


  snowcrab.habitat.db.old = function( ip=NULL, DS="", model.type="gam.full", vclass=NULL, env.init=NULL, yrs=NULL, p=NULL ) {
        
    outdir = file.path( project.directory("snowcrab"), "R", "habitat", "predictions" )
    dir.create(path=outdir, recursive=T, showWarnings=F)
    
    vclass = p$habitat.templates
   
    if ( DS %in% c("habitat", "habitat.redo" )) {
      fn =  file.path( outdir, paste( "predictions", "habitat", vclass, model.type, yrs, "rdata", sep=".") )
      if ( DS == "habitat" ) {
        load(fn)
        return (pH)
      }
      
      # ip is the first parameter passed in the parallel mode
      if (!is.null(env.init)) for( i in env.init ) source (i)
      if (is.null(ip)) ip = 1:length(yrs)
      ip = as.numeric(ip)

    
      require(mgcv)
      H = model.habitat( model.type=model.type, p=p )
      min.yr = min( H$model$yr)
      ref.yr = max( H$model$yr)
      for (iy in ip ) {
        y = yrs[iy]
        PS = habitat.db ( DS="complete", year=y, p=p )
        if ( y < min.yr )  PS$yr = ref.yr # recode year to allow predictions
        pH = predict( H, newdata=PS, se.fit=T, type="response")
        fn =  file.path( outdir, paste( "predictions", "habitat", vclass, model.type, y, "rdata", sep=".") )
        print(fn)
        save( pH, file=fn, compress=T)
      }
      return( "Complete" )
    }
  

    if (DS %in% c("PS", "PS.redo" )) {
     
      if (DS =="PS") {
				fn = file.path( outdir, paste("PS", yrs, vclass, sep=".") )
        load(fn)
        return(PS)
      }  
      
      # ip is the first parameter passed in the parallel mode
      if (!is.null(env.init)) for( i in env.init ) source (i)
      if (is.null(ip)) ip = 1:length(yrs)
      ip = as.numeric(ip)


       for (iy in ip ) {
        yr = yrs[iy]
        fn = file.path( outdir, paste("PS", yr, vclass, sep=".") )
        print (fn)
       
        PS = habitat.db ( DS="complete", year=yr, p=p )
        PS = fishing.area.designations( PS, type="planar")
        PS$cfa.factor = NULL

        pH = snowcrab.habitat.db (DS="habitat", yrs=yr, model.type=model.type, vclass=vclass )
        PS$habitat = pH$fit
        PS$habitat.se = pH$se.fit

        # create labels for merging gridded fisheries data
        PS$gridid = paste(
          PS$plon%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution,
          PS$plat%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution,
          sep="."
        )
   
        # fisheries data regridded
        fg = logbook.db( DS="logbook.gridded", p=p, years=yr )
        fg$gridid = as.character( fg$gridid )
        PS$order = 1:nrow(PS)
        PS = merge(PS, fg, by="gridid", all.x=T, all.y=F, sort=F)
        PS$gridid = NULL # no longer needed
        PS = PS[ sort( PS$order), ]
        PS$order= NULL

        for (vl in setdiff(names(fg), c("plon", "plat","gridid") )) {
          PS[!is.finite(PS[,vl]),vl] = 0 # make sure NA's created by merge statement are set to 0
        }
        rm (fg); gc()
       
        PS$z.annual =NULL
        #  PS$t ## this is the annual mean
        #  PS$tmean  ## this is the climatological mean (1950-present)

        save ( PS, file=fn, compress=T )
      }
      return ("Complete")
    }


  }

 

    count.nonzeros = function(x, d) {
    # used when apply does not work due to memory requirements
        dims = dim(x)
        if (d==1) {
          out = rep( NA, dims[1] )
          for ( i in 1:dims[1] ) {
            out[i] = length(which(x[i,]>0))
          }
        }
        if (d==2) {
          out = rep( NA, dims[2] )
          for ( i in 1:dims[2] ) {
            out[i] = length(which(x[,i]>0))
          }
        } 
        return(out)
      }


# ----------


    count.gt = function(x, d, m ) {
    # used when apply does not work due to memory requirements
        dims = dim(x)
        if (d==1) {
          out = rep( NA, dims[1] )
          for ( i in 1:dims[1] ) {
            out[i] = length(which(x[i,]>m))
          }
        }
        if (d==2) {
          out = rep( NA, dims[2] )
          for ( i in 1:dims[2] ) {
            out[i] = length(which(x[,i]>m))
          }
        } 
        return(out)
      }


# ----------



# ----------

# ----------

# ----------




  habitat.prediction.complete.retired  = function( year, varclass, H, nsims=1, from.file=T ) {
    # form prediction surface in planar coords for snowcrab habitat
  
    outfile =  file.path( project.directory("snowcrab"), "R", "habitat", paste( "PS", year, varclass, "rdata", sep= ".") )
    if ( from.file ) {
      load( outfile )
      return (PS)
    }
    
     out = NULL
        
      PS = habitat.db ( year )
      PS = PS[ which( is.finite(rowSums(PS) )),]
      PS$Y = 1 # required to run "model.matrix"
      X = model.matrix( formula(H), data=PS )
      S = simulate.binomial( X, nsims, H )  # this requires a lot of memory .. watch out 2.6 GiB

      sums = colSums( S )
      sa = mean( sums  )
      sa.sd = sd( sums )
      PS$habitat.sim = rowSums(S) / nsims
      rm (S, X); gc()
      pp = predict( H, PS, type="response", se.fit=T )
      PS$habitat = pp$fit
      PS$habitat.se = pp$se.fit
      rm( pp); gc()
      ii = which(PS$habitat>0.5)
      out = cbind( 
        year, mean(PS$habitat,na.rm=T), sd(PS$habitat,na.rm=T), sa, sa.sd, mean(PS$t[ii],na.rm=T), sd(PS$t[ii],na.rm=T), 
        mean(PS$z[ii],na.rm=T), sd(PS$z[ii],na.rm=T), mean(PS$substrate.mean[ii],na.rm=T), sd(PS$substrate.mean[ii], na.rm=T)   
      ) 
      print(year)
    
     out = as.data.frame(out)
     names(out) = c("year", "habitat.prob.mean", "habitat.prob.sd", "sa", "sa.sd", "t.habitat.mean", "t.habitat.sd", "z.habitat.mean", "z.habitat.sd", "substrate.mean", "substrate.sd" )

     save (PS, file=outfile, compress=T )
     return( out )
  }


