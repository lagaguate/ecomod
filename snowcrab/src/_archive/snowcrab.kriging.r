




  ####-------------
  # --------------------------------------------------

  retired.get.habitat.mask = function( variable="totno.all", y=NULL, fname=NULL, subvar=NULL, DS="file", params=NULL) {

    with( params, {

    if (DS=="file") {
      if ( is.null(subvar) ) {
        print ("Error" )
        stop
      }
      fname = paste(fname, subvar, y, variable, "rdata", sep=".")
      load (fname)
      if (subvar=="Q") return(Q)
      if (subvar=="Z") return(Z)
      if (subvar=="P") return(P)
    }

    if (DS=="redo") {
      trange = get.trange(variable)
      zrange = get.zrange(variable)
      dzrange = get.dzrange(variable)
      ddzrange = get.ddzrange(variable)

      M.SS = mask.grid( loc="ScotianShelf", depthrange=c(5, 500) )

      Q = temperature.grids (y=y, loc="grids/Q", DS="file", fname="tgrid", params=params )
      M.QSS = ifelse( is.finite(Q), 1, NA) # identify the domain of the Scotian Shelf
      M.SS = M.SS * M.QSS
      rm(M.QSS);gc()

  # ----------------------------------------------------
  # MSS is now complete ... no more additions to the mask beyond this level
  # ----------------------------------------------------

    P = M.SS 
    
    DZ = M.SS *  bathymetry.db( DS="dZ.lonlat.grid" ) 
    MDZ  = ifelse( DZ < dzrange[1] | DZ > dzrange[2], NA, 1)
    rm (DZ); gc()
    P = P * MDZ
    rm (MDZ); gc()

    DDZ =  bathymetry.db( DS="ddZ.lonlat.grid") 
    DDZ = DDZ * M.SS
    MDDZ  = ifelse( DDZ < ddzrange[1] | DDZ > ddzrange[2], NA, 1)
    rm (DDZ); gc()
    P = P * MDDZ
    rm (MDDZ); gc()
   

    Q = Q * M.SS
    MQ  = ifelse( Q < trange[1] | Q > trange[2], NA, 1)
    Q = Q * MQ
    P = P * MQ
    rm (MQ); gc()
    
    Z =  bathymetry.db( DS="z.lonlat.grid") 
    Z = Z * M.SS
    MZ  = ifelse( Z < zrange[1] | Z > zrange[2], NA, 1)
    Z = Z * MZ
    P = P * MZ
    rm (MZ); gc()

    rm (M.SS); gc()

#      pl = image( P )
#      Pr(dev="png", dname=file.path(R.sc, "habitat", "map"), fname=y, width=8, height=6)

#      Q.P = Q * P
#      pl = image.plot( Q.P,  zlim=trange )
#      Pr(dev="png", dname=file.path(R.sc, "habitat", "map.Q"), fname=y, width=8, height=6)

#      Z.P = Z * P
#      pl = image.plot( Z.P,  zlim=zrange )
#      Pr(dev="png", dname=file.path(R.sc, "habitat", "map.Z"), fname=y, width=8, height=6)

#      Q.SS = Q * M.SS
#      pl = image.plot( Q.SS, zlim=c(-1,10))
#      Pr(dev="png", dname=file.path(R.sc, "Q" ,"map.lt.10C"), fname=y, width=6, height=4)


    
#      pl = image( P )
#      Pr(dev="png", dname=file.path(R.sc, "habitat", "map"), fname=y, width=8, height=6)

#      Q.P = Q * P
#      pl = image.plot( Q.P,  zlim=trange )
#      Pr(dev="png", dname=file.path(R.sc, "habitat", "map.Q"), fname=y, width=8, height=6)

#      Z.P = Z * P
#      pl = image.plot( Z.P,  zlim=zrange )
#      Pr(dev="png", dname=file.path(R.sc, "habitat", "map.Z"), fname=y, width=8, height=6)

#      Q.SS = Q * M.SS
#      pl = image.plot( Q.SS, zlim=c(-1,10))
#      Pr(dev="png", dname=file.path(R.sc, "Q" ,"map.lt.10C"), fname=y, width=6, height=4)

    # do a histogram of all of scotian shelf temperatures by year ..
#      pl = plot(density( Q, bw=0.2, kernel="gaussian", na.rm=T), xlim=c(-1,10),
#           main=paste(y), cex=5, lty="solid", lwd=5, xlab="Temperature (C)", ylab="Density"  )
#      Pr(dev="png", dname=file.path(R.sc,"Q", "histograms"), fname=y)

#      pl = image.plot( Q, zlim=trange)
#      Pr(dev="png", dname=file.path(R.sc,"Q","map"), fname=y, width=6, height=4)

    # depths
#      x11();
#      plot(density(-Z, bw=0.2, kernel="gaussian", na.rm=T), xlim=c(10,500),
#           main=paste(y), cex=5, lty="solid", lwd=5, xlab="Depth (m)", ylab="Density"  )
#      Pr(dev="png", dname=file.path(R.sc, "Z", "histograms"), fname=y)
#      dev.off()

#      x11();
#      image.plot( -Z )
#      Pr(dev="png", dname=file.path(R.sc, "Z","map"), fname=y, width=6, height=4)
#      dev.off()

    # save the files

      loc = dirname(fname)
      dir.create(path=loc, recursive=T, showWarnings=F)
      suffix = paste( y, variable, "rdata", sep=".")
      q.f = paste(fname, "Q", suffix, sep=".")
      z.f = paste(fname, "Z", suffix, sep=".")
      p.f = paste(fname, "P", suffix, sep=".")

      save(Q, file=q.f, compress=T)
      save(Z, file=z.f, compress=T)
      save(P, file=p.f, compress=T)

    return(NULL)
    }
  }) # end with
}




  retired.prediction.grids = function( p=NULL, make.QZP=F, make.prediction.locations=F, make.prediction.locations.xyz=F, vars=NULL, init.files=NULL, newyears=NULL  ) {
      if (!p$do.parallel) {
          prediction.grids.core( p=p, make.QZP=make.QZP,
            make.prediction.locations=make.prediction.locations,
            make.prediction.locations.xyz=make.prediction.locations.xyz, vars=vars, init.files=init.files, newyears=newyears  )
        } else {
          nelements = ifelse( is.null(newyears), length(p$yearswithTdata), length(newyears) )
          pp = prep.parallel.run(p$clusters, nelements )
          clusterApplyLB( pp$cl, pp$ssplt, prediction.grids.core, p=p, make.QZP=make.QZP,
            make.prediction.locations=make.prediction.locations,
            make.prediction.locations.xyz=make.prediction.locations.xyz, vars=vars, init.files=init.files, newyears=newyears  )
          stopCluster(pp$cl)
        }
    return("completed prediction.grids" )
  }


  retired.prediction.grids.core = function( id=NULL, p=NULL, make.QZP=F, make.prediction.locations=F, make.prediction.locations.xyz=F, vars=NULL, init.files=NULL, newyears=NULL ) {

#   construct Q (temperature), Z (depth), P (prediction locations) data and histograms for
#      the areas of "optimal" snowcrab habitat (lon, lat)
#   prediction surface locations (PS) in planar coords (plon, plat)
  
    for (i in init.files) source( i )

    if( is.null(newyears) ) {
      tyears = p$yearswithTdata
    } else {
      tyears = newyears
    }

    id = c(1:length(tyears))

    if (p$do.parallel) id = as.numeric(id)

      for ( ii in id ) {
        y = tyears[ii]
        if (!p$do.parallel)  print(y)
        
        for (v in vars) {
          p.fname = file.path(R.sc,"grids", "habitat", "habitat")
          
          if (make.QZP) {
            get.habitat.mask( v, y, fname=p.fname, DS="redo", params=p )
            gc()
          }
          
          if (make.prediction.locations) {
            # load prediction surface in (lon,lat) coords
            P = get.habitat.mask ( v, y, fname=p.fname, subvar="P", DS="file", params=p )
            P.xy = grid2xyz(P, p$lons, p$lats)
            rm(P); gc()
            P.xy = P.xy[ is.finite(P.xy$z) ,]
            # re-grid to planar coords at correct resolution
            g.fname = paste( file.path( R.sc, "grids", "habitat", "predict" ), y, v, "rdata", sep=".")
            get.gridded.prediction.surface (DS="redo", P=P.xy, fname=g.fname, params=p, proj.type=p$internal.projection )
            rm( P.xy); gc()
          }

          if (make.prediction.locations.xyz) {
            # load prediction surface in (lon,lat) coords
            P = get.complete.habitat ( v, y, fname=p.fname, DS="redo", params=p )
            rm(P); gc()
          }
        }
      }
    return (NULL)
  }



  retired.get.complete.habitat = function( variable="totmass.all", y=NULL, fname=NULL, DS="file", params=NULL) {

    with( params, {

      if (DS=="file") {
        load (fname)
        return(H)
      }

      if (DS=="redo") {
        # identify the domain of the Scotian Shelf
        M.SS = mask.grid( loc="ScotianShelf", depthrange=c(5, 500) )

        Q = temperature.grids (y=y, loc=file.path(R.sc, "grids", "Q"), DS="file", fname="tgrid", params=params )
        Q = Q * M.SS
        Q = grid2xyz(Q, lons, lats)
        names(Q)=c("lon","lat","t")

        Z = bathymetry.db( DS="z.lonlat.grid" )
        attr(Z, "dimnames") = NULL
        Z = Z * M.SS
        Z = grid2xyz(Z, lons, lats)
        names(Z)=c("lon","lat","z")
        
        H = cbind(Q, z=Z$z);
        rm (Q,Z); gc()

        DZ = bathymetry.db( DS="dZ.lonlat.grid" ) 
        DZ = DZ * M.SS
        DZ = grid2xyz(DZ, lons, lats)
        names(DZ)=c("lon","lat","dz")
        
        H = cbind(H, dz=DZ$dz);
        rm (DZ); gc()
        
        DDZ = bathymetry.db( DS="ddZ.lonlat.grid" ) 
        DDZ = DDZ * M.SS
        DDZ = grid2xyz(DDZ, lons, lats)
        names(DDZ)=c("lon","lat","ddz")

        H = cbind(H, ddz=DDZ$ddz);
        rm (DDZ); gc()
        rm (M.SS); gc()

        H = lonlat2planar( H, proj.type=internal.projection )
        H = H[ is.finite(H[,"z"]) & is.finite(H[,"t"]), c("plon", "plat", "t", "z", "dz", "ddz")]
        H = H[H$z<500 & H$z>0,]
        H = block.xyz2xyz( xyz=H, params=params, engine="R2",  variables=c("t","z", "dz", "ddz") )
        H = H[ is.finite(H[,"z"]) & is.finite(H[,"t"]) ,]

      # save the files
        loc = dirname(fname)
        dir.create(path=loc, recursive=T, showWarnings=F)
        suffix = paste( y, variable, "rdata", sep=".")
        f = paste(fname, "H", suffix, sep=".")
        save(H, file=f, compress=T)
        print (f)
        return(NULL)
      }

    }) # end with params
  }

  retired.get.gridded.prediction.surface = function( DS=NULL, P=NULL, fname=NULL, params=NULL, proj.type=NULL ) {

    if (DS=="file") {
      load (fname)
      return(P.xy)
    }

    if (DS=="redo") {
      with (params, {
        P.planar = lonlat2planar( P, proj.type)   # grid2xyz is cpu expensive .. find another solution
        P.planar = P.planar[ is.finite(P.planar[,"z"]) ,c("plon", "plat", "z")]
        P.regridded = block.xyz2xyz( xyz=P.planar, params=params, engine="R2",  variables=c("z") )
        P.regridded = P.regridded[ is.finite(P.regridded[,"z"]) ,]
        rm(P.planar); gc()
        P.xy = P.regridded[, c("plon", "plat")]
        rm(P.regridded ); gc()
        dir.create( dirname(fname), showWarnings=F)
        save(P.xy, file=fname, compress=T)
      }) # end with params

      return(NULL)
    }
  }

  # -------------------------



# ----------------------------------

retired.interpolated.estimates = function( flags, set ) {

  with (flags, {
  B.out = NULL

  for (y in years.to.model ) {
  for (v in vars) {
    C = set[, c("lon", "lat", v)]
    C[,v] = variable.recode( C[,v], v, direction="forward", db="snowcrab" )  # normalise the data where required
    C = C[ which(C$yr==y) ,]

    if (make.biomass.grids) {
      if ( p$interp.method =="tps") {
        p$tension = "-T0.75"  # 0.35+ for steep; 0.25 for smooth
        p$maskres = "-S20k"
        p$interpres = "-S20k"
      }
      if ( p$interp.method =="inv.dist.gstat") {
        p$drange = 150  # distance range of variograms (km)
        p$nmax = 50
        p$power = 0.5  # power of the inverse distance used for interpolation
      }
      interp.xyz2grid ( y, C, lons, lats, loc=file.path(R.sc,"grids/B"), DS="redo", fname=file.path(R.sc,"bgrid"), params=p )
    }

    if (biomass.regrid) {
      # regrid biomass surface in (lon,lat) coords to planar
      B = interp.xyz2grid ( y, loc="grids/B", DS="file", fname=file.path(R.sc,"bgrid"), params=p )
      B.xy = grid2xyz(B, lons, lats)
      B.xy = B.xy[ is.finite(B.xy$z) ,]
      fname = paste( file.path(R.sc,"grids","B", "estimates"), p$interp.method, y, v, "rdata", sep=".")
      get.gridded.planar.surface (DS="redo", B=B.xy, fname=fname, params=p, proj.type=p$internal.projection)
      print(paste(v,y))
      rm(B); gc()
    }

    if ( biomass.extract) {
      C = set
      C = C[ which(C$yr==y), c("lon", "lat", v)]
      bmax = max( C[,v], na.rm=T)
      bmin = min( C[C[,v]>0,v], na.rm=T)

      # load prediction surface  .. force for "totno.all"
      PS.fname = file.path(R.sc, "grids", "habitat", paste( "predict", y, "totno.all", "rdata", sep="."))
      PS = get.gridded.prediction.surface (DS="file", fname=PS.fname)
      
      ##!!!!!!! ---- obtain PS from new (modelled) data series 
      
      
      BE = get.gridded.planar.surface (fname=paste( file.path(R.sc,"grids", "B", "estimates"), p$interp.method, y, v, "rdata", sep="."), DS="file")
      B = merge(x=PS, y=BE, by=c("plon", "plat"), all.x=T, all.y=F, sort=F)
      B$z.m = variable.recode( x=B$z.m, v, direction="backward", db="snowcrab" )
      B$z.m[ B$z.m < 0 ] = NA
#        B$z.m[ B$z.m>bmax ] = NA
#        B$z.m[ B$z.m<bmin ] = NA
      B  = B[ is.finite(B$z.m) ,]

      for (r in regions) {
        i = filter.region.polygon(x=B[, c("plon", "plat")], region=r, planar=T)
        Bs = sum( B[i, "z.m"], na.rm=T )
        Br = data.frame( cbind(var=I(v), region=I(r), yr=as.numeric(y), B=round(as.numeric(Bs)/100, 0)*100 ) )
        B.out = rbind( B.out, Br )
      }
      print (B.out)
    }
  }
  }
  B = B.out
  save(B, file=flags$outfile, compress=T)

  }) # end with flags
  return( B )
}

# -----------------------------------------------------------

retired.interp.xyz2grid = function( y, G=NULL, lons=NULL, lats=NULL, DS="file", loc=NULL,
                         fname=file.path(R.sc,"G"),  params=NULL ) {
  # this function is a wrapper to interpolate and call "xyz2grid" that also allows data to be saved and recalled

  with (params, {

    rname = file.path(loc, fname)
    bgrid = NULL

    if (DS=="file") {
      load (paste(rname, y, interp.method, "rdata", sep=".") )
      return (G)
    }

    if (DS=="redo") {
      out = NULL
      G = G[is.finite( rowSums( G ) ) ,]
      if (interp.method=="tps") {
          G = interpol.grid(xyz=G, params=params, getdata=T)
          G = xyz2grid(xyz=G, xx=lons, yy=lats)
      }

      if (interp.method=="inv.dist.gstat") {
        # inverse distance weighted interpolation
        G = lonlat2planar( G, proj.type=params$internal.projection)
        locs = make.surface.grid(list(lons ,lats)) # required for plotting and interpolations in library "fields"
        G = interpol.grid(xyz=G, locs=locs, params=params, getdata=T)
        G = xyz2grid(xyz=G, xx=lons, yy=lats)
      }

      fname.out = paste(rname, y, interp.method, "rdata", sep=".")
      loc = dirname( fname.out )
      dir.create( path=loc, recursive=T, showWarnings=F )
      save( G, file=file.path(loc, fname.out), compress=T)


      print ( paste("Completed", fname.out) )

    }

 }) #end with params
 return("Done")

}



  retired.get.gridded.planar.surface  = function( DS=NULL, B=NULL, fname=NULL, params=NULL, proj.type=NULL ) {

  if (DS=="file") {
    load (fname)
    return(B.xy)
  }

  if (DS=="redo") {
    with( params, {
      B.planar = lonlat2planar( B, proj.type )   # grid2xyz is cpu expensive .. find another solution
      B.planar = B.planar[ is.finite(B.planar[,"z"]) ,c("plon", "plat", "z")]
      B.regridded = block.xyz2xyz( xyz=B.planar, params=params, engine="R2",  variables=c("z") )
      rm(B.planar);gc()
      B.regridded = B.regridded[ is.finite(B.regridded[,"z.m"]) ,]
      B.xy = B.regridded[, c("plon", "plat", "z.m")]
      rm(B.regridded);gc()
      dir.create( dirname(fname) )
      save(B.xy, file=fname, compress=T)
    }) # end with params

    return(NULL)
  }
}


retired.trawled.areas = function ( set, years, loc, extent=20, lons, lats, res, DS="file", fname) {

  rname = file.path(loc, fname)
  
  # moncton added areas within 10 minutes of trawled areas to part of the potential kriging surface
  # ... use the saved datafile unless it is a new year/altered sampling areas 
  # .. this has been increased to 20 min blocks


  if (DS=="file") {
    load( paste(rname, years, extent, "rdata", sep="."))
    return (trawledareas)
  }

  if (DS=="redo") {
    for (y in years) {
    
      fname = file.path(loc, paste(fname, extent, "rdata", sep="."))
      y = set[ which(set$yr==y), c("lon", "lat")]
      y = y[is.finite(rowSums(y)),]
      q = unique(y)
      min = extent
      a = seq(-min/60, min/60, res)
      box = as.matrix(expand.grid(a, a))
      grid = xyz2grid(xyz=data.frame(cbind(lons[1],lats[1])), xx=lons, yy=lats, fill=0 ) # get it started
      for (i in 1:dim(q)[1]) {
        d1 = box[,1] + q[i,1]
        d2 = box[,2] + q[i,2]
        grid = grid + xyz2grid(xyz=data.frame(cbind(d1,d2)), xx=lons, yy=lats, fill=0 )
      }
      grid[which(grid >0)] = 1
      grid[which(grid==0)] = NA

      trawledareas = grid

      loc = dirname( fname )
      dir.create( path=loc, recursive=T, showWarnings=F )
      save( trawledareas, file=file.path(loc,fname), compress=T)



      trawledareas.xyz = grid2xyz (trawledareas, lons, lats)
      trawledareas.xyz = trawledareas.xyz[ filter.region.polygon(
                  x=trawledareas.xyz[, c("lon", "lat")], region="cfaall", planar=F),]
      trawledareas = xyz2grid(trawledareas.xyz, lons, lats)

      fname.out = paste(rname, y, extent, "rdata", sep=".")
      loc = dirname( fname.out )
      dir.create( path=loc, recursive=T, showWarnings=F )
      save( trawledareas, file=file.path(loc, fname.out), compress=T)
      print(y)
    }
  return ("completed")
  }
}


retired.create.temperature.data = function( flags) {

    pcoords = c("plon", "plat")
    out = NULL

    v = "totno.all"
    trange = get.trange(v)
    zrange = get.zrange(v)
    dzrange = get.dzrange(v)
    ddzrange = get.ddzrange(v)

       for (y in flags$tempyears) {

         PS.fname = file.path(R.sc, "grids", "habitat",  paste( "habitat", "H", y, "totno.all", "rdata", sep=".") )
         PS = get.complete.habitat (DS="file", fname=PS.fname)
         
###### get from new method


         PS = PS[PS$z > 0, ] # some strange data .. temp fix
         PS$gridid = paste( 
           PS$plon%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution, 
           PS$plat%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution, 
           sep="." 
         )

    PS.to.remove.z = which( PS$z < zrange[1] |  PS$z > zrange[2] )
    PS.to.remove.t = which( PS$t < trange[1] |  PS$t > trange[2] )
    PS.to.remove.dz = which( PS$dz < dzrange[1] |  PS$dz > dzrange[2] )
    PS.to.remove.ddz = which( PS$ddx < ddzrange[1] |  PS$ddz > ddzrange[2] )

    PS.to.remove = unique( c( PS.to.remove.z, PS.to.remove.t, PS.to.remove.dz, PS.to.remove.ddz ) )
    rm(PS.to.remove.ddz, PS.to.remove.dz, PS.to.remove.t, PS.to.remove.z ); gc()
    
    PS = PS[-PS.to.remove,]
    rm(PS.to.remove ); gc()


         totalsurfacearea = dim(PS)[1] * (p$pres*p$pres) 

         for (r in flags$regions) {

           i = filter.region.polygon(x=PS[, pcoords], region=r, planar=T)
           meantemp = NA
           surfacearea = length(i) * (p$pres*p$pres) 

           if ( length(i) > 3) {
              meantemp = mean(PS$t[i], na.rm=T)
              sdtemp = sd(PS$t[i], na.rm=T)
           }
           res = NULL
           res = data.frame( cbind(yr=y, region=r, totalsurfacearea=totalsurfacearea, surfacearea=surfacearea, meantemp=meantemp, sdtemp=sdtemp) )
           out = rbind( out, res )
        }
      }
    out = factor2number (out, c("yr", "totalsurfacearea", "surfacearea", "meantemp", "sdtemp") )
    out = factor2character (out, c("region") )
    return( out )
  }


get.zrange = function(v=NULL) {
#    zrange =               cbind("totmass.male.com", -300, -60)
#    zrange = rbind(zrange, cbind("totmass.male.mat", -300, -60) )
#    zrange = rbind(zrange, cbind("totmass.male.imm", -300, -60) )
#    zrange = rbind(zrange, cbind("totmass.female.imm", -300, -60) )
#    zrange = rbind(zrange, cbind("totmass.female.mat", -300, -60) )
#    j = which(zrange[,1] == v)
#    if (length(j)==1) {
#      depthrange = as.numeric(zrange[j, c(2,3)])
#    } else  {
    depthrange = c(60, 280)
#    }
 return (depthrange)
}

# -------------------------------------------------------------------

get.ddzrange = function(v) {
  ddz = c(9.5, 13.5) # log (abs( curvature ) )
 return (ddz)   
}

# -------------------------------------------------------------------

get.dzrange = function(v) {
  dz = c(-7, -3) # log (abs( slope ) )
 return (dz)
}

# -------------------------------------------------

get.trange = function(v=NULL) {
#    trange =               cbind("totmass.male.com", -1, 6)
#    trange = rbind(trange, cbind("totno.male.mat", -1, 6) )
#    trange = rbind(trange, cbind("totno.male.imm", -1, 6) )
#    trange = rbind(trange, cbind("totno.female.imm", -1, 6) )
#    trange = rbind(trange, cbind("totno.female.mat", -1, 6) )
#    j = which(trange[,1] == v)
#    if (length(j)==1) {
#      temprange = as.numeric(trange[j, c(2,3)])
#    } else  {
    temprange = c(-1, 6)
#    }
 return (temprange)
}


get.mastergrid.xy = function(x, y, varnames=c("x", "y"), resolution, DS="file", loc="grids", fname="xy") {
  fname = file.path(loc, paste(fname, resolution, "rdata", sep="."))
  if (DS=="redo") {
    xy = expand.grid(x=x, y=y)
    attr(xy, "out.attrs") = NULL  # remove uneeded data
    names(xy) = varnames

    loc = dirname( fname )
    dir.create( path=loc, recursive=T, showWarnings=F )
    save( xy, file=file.path(loc,fname), compress=T)

  }
  if (DS=="file") load (fname)
  return(xy)
}


  retired.get.gridded.fisheries.data  = function( p, redo=F, single.year=NULL ) {

  if (!is.null(single.year)) {
    datayears=single.year
  } else {
    datayears=p$years.to.model
  }

  for (fishing.year in datayears) {

    loc = "gridded.fishery.data"
    dir.create(path=loc, recursive=T, showWarnings=F)
    outfilename = paste( "gridded.fishery.", fishing.year, ".rdata", sep="")
    outfile = file.path(loc, outfilename)

    if (redo) {
      # load logbook info: global summary

      fg = logbook.db( DS="fishing.grounds.global" )  # in dataframe fg

      fg0 = regrid.lonlat(old=fg, res=p$fisheries.grid.resolution, vr.to.sum=c("total.landings", "total.effort", "total.visits"))
      fg0$core.visits0 = ifelse( fg0$total.visits >= 5, 1, 0 )
      fg0$total.cpue = log( fg0$total.landings / fg0$total.effort + 1)
      fg0$total.landings = log( fg0$total.landings+1 )
      fg0$total.effort = log( fg0$total.effort+1 )
      fg0$total.visits = log( fg0$total.visits+1 )
      fg0 = fg0[ , c( "gridid", "core.visits0", "total.effort", "total.cpue", "total.landings", "total.visits" ) ]
      rm(fg)

      # load logbook info: annual
      fg = logbook.db( DS="fishing.grounds.annual" )  # in dataframe fg
      fg = fg[ which(fg$yr==fishing.year),]
      fg = regrid.lonlat(old=fg, res=p$fisheries.grid.resolution, vr.to.sum=c("total.landings", "total.effort", "total.visits") )
      fg$core.visits = ifelse(fg$total.visits >= 3, 1, 0)
      fg$core.visits = ifelse(fg$total.visits >= 3, 1, 0)
      fg$core.landings = ifelse(fg$total.landings >= 5, 1, 0)
      fg$core.effort = ifelse(fg$total.effort >= 100, 1, 0)
      fg$cpue = log( fg$total.landings / fg$total.effort + 1)
      fg$landings = log( fg$total.landings+1 )
      fg$effort = log( fg$total.effort+1 )
      fg$visits = log( fg$total.visits +1 )

      fg = fg[ , c( "gridid", "core.visits", "core.landings", "core.effort", "cpue", "landings", "effort", "visits" ) ]
      fg = fg[ is.finite(fg$cpue * fg$landings * fg$effort),]
      fg = fg[ which(fg$core.visits==1) , ]

      gridded.fishery.data = merge(fg0, fg, by="gridid", all.x=T, all.y=T, sort=F)
      save(gridded.fishery.data, file=outfile, compress=T)
    } else {
      load( outfile )
    }
  } # end for

    return(gridded.fishery.data)
  }

 

  get.PS.S.gridded = function(p, y, v, vars.subset=NULL ) {
    
    pcoords = c("plon", "plat")
    base.vars = c("yr", pcoords, "t", "z")
   
    degrees.Kelvin = 273.15

    # extract variables from kriging formula by default
    if ( is.null( vars.subset) ) vars.subset = unlist(strsplit(gsub(" ", "", gsub("~", "+", deparse(p$kformula))),"+", fixed=T)) 
  
    trange = get.trange(v)
    zrange = get.zrange(v)
    dzrange = get.dzrange(v)
    ddzrange = get.ddzrange(v)
 
    # load prediction surface  .. currently force to use one global solution: "totno.all" as they are comparable
    PS.fname =  file.path(R.sc, "grids" ,"habitat", paste("habitat", "H", y, "totno.all", "rdata", sep=".") )
    PS = get.complete.habitat (DS="file", fname=PS.fname)
    
    # _____ obtain from new method
    
    
    
    PS = PS[, which( colnames(PS) %in% c( pcoords, vars.subset ) ) ]
    PS = PS[ which( PS$z > 0 ),] # some strange data .. temp fix
    
    set = snowcrab.db("set")
    set = set[ which(set$yr==y), pcoords]  
     
    distances =  rdist(PS[,pcoords], set)
    rm(set); gc()
    distances[ which(distances < p$threshold.distance) ] =  NA
    PS.to.keep = sort( which( !is.finite( rowSums(distances) ) ) )
    rm(distances); gc()

    # create labels for merging gridded fisheries data
    PS$gridid = paste( 
      PS$plon%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution, 
      PS$plat%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution, 
      sep="." 
    )

    PS0 = PS[, c("plon", "plat", "t", "z")]
    PS0 = rename.df(PS0, "t", "tgridded")
    PS0 = rename.df(PS0, "z", "zgridded")

    PS.to.remove.z = which( PS$z < zrange[1] |  PS$z > zrange[2] )
    PS.to.remove = PS.to.remove.z
    rm(PS.to.remove.z); gc()

    PS.to.remove.t = which( PS$t < trange[1] |  PS$t > trange[2] )
    PS.to.remove = sort( c(PS.to.remove, PS.to.remove.t) )
    rm(PS.to.remove.t); gc()

    PS.to.remove.dz = which( PS$dz < dzrange[1] |  PS$dz > dzrange[2] )
    PS.to.remove = sort( c(PS.to.remove, PS.to.remove.dz) )
    rm(PS.to.remove.dz); gc()

    PS.to.remove.ddz = which( PS$ddx < ddzrange[1] |  PS$ddz > ddzrange[2] )
    PS.to.remove = sort( c(PS.to.remove, PS.to.remove.ddz) )
    rm(PS.to.remove.ddz); gc()

    PS.to.remove = sort( setdiff( PS.to.remove, PS.to.keep ) )
    PS = PS[-PS.to.remove,]
    rm(PS.to.remove, PS.to.keep ); gc()
   

# correct for nonlinearity of external drift variables
  #  according to GULF region Framework RAP reviewer, this linearlisation may not be needed
  
  
  if (p$linearise.drift.terms) {
   # from visual inspection (assuming a symmetrical distribution)
    m = which( PS$z > p$inflection.z )
    PS$z[m] = 50
    
    # from visual inspection (assuming a symmetrical distribution) 
    m = which(PS$t > p$inflection.t )
    PS$t[m] = p$inflection.t - (PS$t[m] - p$inflection.t)
  }
    PS$z = log(PS$z)  # depth follows lognormal relationships
    PS$t = 1/( PS$t + degrees.Kelvin )  # this linearises many enzynamtic functions

    # fisheries data regridded
    fg = logbook.db( DS="gridded.fisheries", p=p, years=y )
    fg$gridid = as.character( fg$gridid )
    fg = fg[, which( colnames(fg) %in% c("gridid", vars.subset) ) ]
    PS = merge(PS, fg, by="gridid", all.x=T, all.y=F, sort=F)
    PS$gridid = NULL # no longer needed
    for (vl in setdiff(names(fg), c(pcoords,"gridid") )) PS[!is.finite(PS[,vl]),vl] = 0 # make sure NA's created by merge statement are set to 0
    rm (fg); gc()

    S = snowcrab.db("set")
    S = S[ , which( colnames(S) %in% c("yr", v, pcoords, vars.subset) ) ]

    # normalise the data where required
    S[,v] = variable.recode( S[,v], v, direction="forward", db="snowcrab" )
    S$plon = floor(S$plon)
    S$plat = floor(S$plat)
    S$gridid = paste(
      S$plon%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution,
      S$plat%/%p$fisheries.grid.resolution * p$fisheries.grid.resolution, 
      sep="." 
    )

    S = merge(S, PS0,  by=pcoords, all.x=T, all.y=F, sort=F)

    no.t = which( !is.finite(S$t) )
    no.z = which( !is.finite(S$z) )
    if (length(no.t)>0) S[no.t,"t"] = S[no.t,"tgridded"]
    if (length(no.z)>0) S[no.z,"z"] = S[no.z,"zgridded"]
    
  
  if (p$linearise.drift.terms) {
       
    m = which( S$z > p$inflection.z )
    S$z[m] = 50

    m = which(S$t > p$inflection.t)
    S$t[m] = p$inflection.t - (S$t[m] - p$inflection.t)
  }
    
    S$z = log(S$z)  # depth follows lognormal relationships
    S$t = 1/( S$t + degrees.Kelvin )  # this linearises many enzynamtic functions

    fg = logbook.db( DS="gridded.fisheries", p=p, years=y )
    fg = fg[, which( colnames(fg) %in% c("gridid", vars.subset) ) ]
    S = merge(S, fg, by="gridid", all.x=T, all.y=F, sort=F)
    for (vl in setdiff(names(fg), c("gridid", pcoords)) ) S[!is.finite(S[,vl]),vl] = 0
    rm (fg); gc()


    S = S[, which( colnames(S) %in% c("yr", v, pcoords, vars.subset) ) ]
    S = rename.df(S, v, "kv")

    CC = S[ which(S$yr==y) ,]
    if (dim(CC)[1] < 10)  break  # must have enough data if we are to krige .. escape mechanism
    mm = which(!is.finite(CC$z))
    if (length(mm)>0)  CC$z[mm] = jitter( rep(mean(CC$z, na.rm=T, trim=0.1), times=length(mm)), amount=0)  # add a little noise to prevent singular solutions
    mm = which(!is.finite(CC$t))
    if (length(mm)>0)  CC$t[mm] = jitter( rep(mean(CC$t, na.rm=T, trim=0.1), times=length(mm)), amount=0)

    return(list(PS=PS, S=S, CC=CC) )

  }

krige.universal = function( id=NULL, p, init.files=NULL, load.from.file=F, v=NULL, y=NULL, r=NULL ) {
    
  for (i in init.files) source( i )
  
  # this is the real call to the gstat kriging prediction engine 
  
  loc = file.path(R.sc, "kriged.solutions")
  dir.create(path=loc, recursive=T, showWarnings=F)
 
  if ( load.from.file ) {
    Koutfilename = paste( "Kriged", p$kriging.type, v, y, r, "rdata", sep=".")
    Koutfile = file.path(loc, Koutfilename)
    print(Koutfile)
    z = NA
    if (file.exists( Koutfile )) load(Koutfile)
    return (z)
  }
  
  # begin kriging predictions

  K = NULL
  pcoords = c("plon", "plat")
  
  # the first index is a list that is passed from the calling prog: in this case "ssplt" (if parallel)
  if ( is.null(id)) id = c(1: p$nruns ) 
  id = as.numeric(id)

  for (i in id ) {
    v = p$runs[i,1]
    y = p$runs[i,2]
    r = p$runs[i,3]
 
    # get data for the whole area
    PS = kriging.db( DS="PS.kriging", yrs=y ,v ) 
    
    S  = kriging.db( DS="set.kriging", y ,v ) 
    S = rename.df(S, v, "kv")
    
    icc = which( S$yr == y )
    if ( length(icc) <10) next()
    CC = S[ icc, ]
     
    totalsurfacearea = dim(PS)[1]
    gc()

    # jitter is used to prevent singular solutions when stations are too close to each other
    CC$plon = jitter(CC$plon)
    CC$plat = jitter(CC$plat)  
    var.norm = var(CC[,"kv"], na.rm=T)  # variance 
    
    # run a global variogram to obtain constants
    gstat.vgm = get.completed.variogram.model (S, var.norm, v, y, p)
    vgm.e0 = gstat.vgm$vgm.e
    vgm.m = gstat.vgm$vgm.m
    if (is.null(vgm.e0) | is.null(vgm.m)) next
    rm(gstat.vgm, S)

    gc()
      
    Koutfilename = paste( "Kriged", p$kriging.type, v, y, r, "rdata", sep=".")
    Koutfile = file.path(loc, Koutfilename)

    # identify the locations for prediction estimates
    o = oo = z = vgm.e = vgm.m = NULL
    o = filter.region.polygon(x=PS[, pcoords], region=r, planar=T)

    n.data =  length(o) 
    if ( n.data<10 ) next # no available habitat space of significance
    surfacearea = n.data * (p$pres*p$pres)

    oo = filter.region.polygon(x=CC[, pcoords], region=r, planar=T)
    if ( length(oo) < 5) ii = c(1:dim(CC)[1]) # if no data assume variance of the global dataset
    
    # rescale the global empirical variogram to the variance in the data in the sub-area and then obtain the variogram model
    var.e = var( CC[oo,"kv"] )
    vgm.e = vgm.e0
    vgm.e$gamma = vgm.e$gamma * var.e
    vgm.m = gstat.model.variogram ( vgm.e, vp=get.variogram.params(v, y) )  # the get.vario .. is a blacklist of unusable variables/ years
    if ( length(vgm.m)==0 ) {
      print( "Kriging problem with:", v, y, r )  
      next  # no viable solutions
    }

    vario.sse = ifelse(is.null(attr(vgm.m, "SSErr")), NA, attr(vgm.m, "SSErr") )
    vario.model = vgm.m$model[2]
    psill = vgm.m$psill[2]
    nugget = vgm.m$psill[1]
    range = vgm.m$range[2]
    
    g = gstat(id=v, model=vgm.m, formula=p$kformula, locations=p$klocs, data=CC, nmax=p$knmax)
    z = K0 =NULL

      if (p$kriging.type=="point") {  
        # point kriging
        z = try( predict(object=g, newdata=PS[o,], debug=-1), silent=T)
      } else if (p$kriging.type=="block") {
        # block kriging
        z = try( predict(object=g, newdata=PS[o,], block=c(p$pres,p$pres), debug=-1), silent=T)
      } else if (p$kriging.type=="block.conditional.sims") {
        # block kriging with Gaussian conditional simulation ("trans-Guassian" sensu Cressie)
        # via direct summation of back-transformed data 
        z = try( predict(object=g, newdata=PS[o,], block=c(p$pres,p$pres), nsim=p$n.conditional.sims, debug=-1), silent=T)
      }
        
       if (class(z)=="try-error") next()

       datacols = c(3:ncol(z))
       z[,datacols] = variable.recode( z[,datacols], v, direction="backward", db="snowcrab" )
       save (z, file=Koutfile, compress=T)
  
        z.sum = apply( z[,datacols], 2, sum, na.rm=T )
        z.sum.sd = sd( z.sum )
        z.sum.mean = mean ( z.sum )
        ci = quantile( z.sum, probs=c(0.025, 0.5, 0.975), na.rm=T,names=F )
        lbound = ci[1]
        ubound = ci[3]
        median = ci[2]
        K0 = data.frame( yr=y, vars=v, region=r, var=var.e, total=z.sum.mean, median, lbound, ubound,
          totalsurfacearea, surfacearea, vario.model, vario.sse, psill, nugget, range )
        if (is.null(K)) {
          K = K0 
        } else {
          K = rbind( K, K0 )
        }
 
    } # runs
    
    if (is.null(K)) {
      K = NA ## calling function assumes a non-null value will be returned
    } else {
      save(K, file= make.random.string(p$ofname) , compress=T)
    }

  return( K )
}


  mask.grid = function ( loc="ScotianShelf", depthrange=c(5, 500) ) {  
    
    if ( loc %in% c("ScotianShelf", "ScotianShelf.redo" ))  {
      maskfile = file.path( project.directory("snowcrab"), "R", "grids", paste( "Mask.SS", depthrange[1], depthrange[2], "rdata", sep=".") )
      if (loc == "ScotianShelf") {
        load( maskfile )
        return (M.SS)
      }
     # begin the masking grid M.SS
        M.SS = subselect.xy2grid (area="cfaall", resolution=mapres, DS="file", loc="grids", fname="mask.cfa")
     # screen out the Bay of Fundy (4X) area
        M.4X = get.boxes ( area="4X", DS="file", fname=file.path( project.directory("snowcrab"), "R", "grids", "mask.4X.rdata" ) )
        M.SS = M.SS * M.4X
        rm(M.4X); gc()
        
        Z = load.bathymetry(DS="file")
        Z = abs(Z)
        attr(Z, "dimnames") = NULL
        M.ZSS = ifelse( Z < depthrange[1] | Z > depthrange[2], NA, 1) # identify the domain of the Scotian Shelf
        M.SS = M.SS * M.ZSS
        rm(M.ZSS);gc()
        save(M.SS, file=maskfile, compress=T)
        return (M.SS)
    }

    if ( loc %in% c("ScotianShelf.planar", "ScotianShelf.planar.redo" ))  {  
      ##!!!!!!!!!!!!!!! not done yet , not needed yet .. just a copy of the above ... !!!!!!!!!!!!!!!!!!
      maskfile = file.path( project.directory("snowcrab"), "R", "grids", paste( "Mask.SS.planar", depthrange[1], depthrange[2], "rdata", sep=".") )
      if (loc == "ScotianShelf.planar") {
        load( maskfile )
        return (M.SS)
      }
     # begin the masking grid M.SS
        M.SS = subselect.xy2grid (area="cfaall", resolution=mapres, DS="file", loc="grids", fname="mask.cfa")
     # screen out the Bay of Fundy (4X) area
        M.4X = get.boxes ( area="4X", DS="file", fname=file.path(project.directory("snowcrab"), "R", "grids", "mask.4X.rdata" ) )
        M.SS = M.SS * M.4X
        rm(M.4X); gc()
        
        Z = bathymetry.db(DS="Z") # <------ must reshape to the right internal coord system 
        Z = abs(Z)
        attr(Z, "dimnames") = NULL
        M.ZSS = ifelse( Z < depthrange[1] | Z > depthrange[2], NA, 1) # identify the domain of the Scotian Shelf
        M.SS = M.SS * M.ZSS
        rm(M.ZSS);gc()
        save(M.SS, file=maskfile, compress=T)
        return (M.SS)
    }

  }


