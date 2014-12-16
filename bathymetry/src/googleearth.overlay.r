  
  # Googleearth overlays: 

  # common functions:
  loadfunctions( c("spatialmethods", "utility", "bathymetry")) 
  
  cmd = function(x, ...) { system(paste(x, ...)) }

	tmpdir = tempdir()

  # 1.  map of bathymetry contours : colour background and lines

  gmt = list()
  gmt$out = file.path(project.directory("bathymetry"), "ss.bathymetry.colour.platecarre.ps" )
  gmt$outputs = c( "colourscale", "colourcontour", "bathymetry.redo" )
  gmt$region=" -R-72/-52/40/50"    
  gmt$projection="-JQ-62/6.5i"  # Cylindrical equidistant (Plate Carre)  the default required by Google Earth
  gmt$resolution="-I0.25m" #  resolution
  gmt$inp = file.path(project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz" )
  gmt$tension ="-T0.35"
  gmt$cpt = "-Cjet -T-350/350/10 -Z -D"
  gmt$incscale = "-B50"
  gmt$font = "14p"
  gmt$annot.base = "-63.7 47.25 24 0 Helvetica LT"  # lon0, lat0, fontsize, angle, font, justification
  gmt$annot.text = "Bathymetry (m)"
  gmt$scale.location = "-D4.75i/0.75i/2.6i/0.16ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
  # gmt$scale.location = "-D4.75i/0.75i/1i/0.1ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
   
  
  setwd( dirname( gmt$out ) )
  gmt.map.simple(gmt)
  cmd( "ps2raster", gmt$out, "-V -Au -P -E300 -Tg -S" ) 
  
#  cmd( "ps2raster", gmt$out, "-V -Au -P -E300 -TG -Qg2 -S" ) 
  



 # 2. map point-kriged data .. used for overlaying onto google-earth to id new stations ---------
 # fishable biomass density
 
  required.libraries = c( 
    "mgcv", "chron", "lattice"
  )
  for ( L in required.libraries) require( L, character.only=T )

  loadfunctions("snowcrab", functionname="initialise.local.environment.r" )

  gmt = list()
  gmt$out = file.path( project.directory("snowcrab"), "maps", "googleearth", "R0.platecarre.ps" )
  gmt$outputs = c( "colourscale", "colourcontour" )
  gmt$region=" -R-72/-52/40/50"    
  gmt$projection="-JQ-62/6.5i"  # Cylindrical equidistant (Plate Carre)  the default required by Google Earth
  gmt$resolution="-I0.25m" #  resolution
  gmt$inp = NULL 
  gmt$tension ="-T1"
 
  gmt$cpt = "-Cseis -T-3/2/0.1 -Z -I -D"
  gmt$incscale = "-B2"
  gmt$font = "14p"
  gmt$annot.base = "-63.7 47.25 24 0 Helvetica LT"  # lon0, lat0, fontsize, angle, font, justification
  gmt$annot.text = "Fishable biomass (log10; kg/km2)"
  gmt$scale.location = "-D4.25i/0.75i/0.75i/0.075ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
  
  gmt$bathy.xyz = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz")
  gmt$bathy.grid = file.path(tmpdir, make.random.string(".gmt.grid"))

  PS = kriging.db( DS="UK.point.PS", p=list(v="R0.mass", y=2008, r="cfaall", transgaussian.kriging=T)  )
  PS$R0.mass.pred = log10( PS$R0.mass.pred ) 
  PS = planar2lonlat( PS, proj.type="utm20" )

  gmt$dat = PS[, c("lon", "lat", "R0.mass.pred" )] 
  
  # determine colour ranges:
  hist( gmt$dat[,3]) # log scale ssems to work best: -3 to 2
 

  gmt.map.simple(gmt)
  
   
   setwd( dirname( gmt$out ) ) # ps2raster has difficulties with different directories

 cmd( "ps2raster", basename(gmt$out), "-V -Au -P -E300 -Tg -S" ) 
#  cmd( "ps2raster", gmt$out, "-V -Au -P -E300 -TG -Qg2 -S" ) 

 # then use Gimp or something like it to replace white with transparent




 # 3. map point-kriged data .. used for overlaying onto google-earth to id new stations ---------
 # SD in fishable biomass
  
  required.libraries = c( 
    "mgcv", "chron", "lattice"
  )
  for ( L in required.libraries) require( L, character.only=T )

  loadfunctions("snowcrab", functionname="initialise.local.environment.r" )

  gmt = list()
  gmt$out = file.path( project.directory("snowcrab"), "maps", "googleearth", "R0.sd.platecarre.ps" )
  gmt$outputs = c( "colourscale", "colourcontour" )
  gmt$region=" -R-72/-52/40/50"    
  gmt$projection="-JQ-62/6.5i"  # Cylindrical equidistant (Plate Carre)  the default required by Google Earth
  gmt$resolution="-I0.25m" #  resolution
  gmt$inp = NULL 
  gmt$tension ="-T1"
  gmt$bathy.xyz = file.path( project.directory("bathymetry"), "data", "bathymetry.canada.east.xyz")
  gmt$bathy.grid = file.path(tmpdir, make.random.string(".gmt.grid"))

  PS = kriging.db( DS="UK.point.PS", p=list(v="R0.mass", y=2008, r="cfaall", transgaussian.kriging=T)  )
  PS = planar2lonlat( PS, proj.type="utm20" )
  PS$R0.mass.sd = sqrt( PS$R0.mass.var )
  gmt$dat = PS[, c("lon", "lat", "R0.mass.sd" )] 
  gmt$dat = gmt$dat[ which( gmt$dat[,3] <=14 ) ,]
  # determine colour ranges:
  hist( gmt$dat[,3] )
  hist( log10( gmt$dat[,3] ) ) # log scale ssems to work best: -3 to 2
  
  gmt$cpt = "-Cseis -T1/12/0.1 -Z -I -D"
  gmt$incscale = "-B2"
  gmt$font = "14p"
  gmt$annot.base = "-63.7 47.25 24 0 Helvetica LT"  # lon0, lat0, fontsize, angle, font, justification
  gmt$annot.text = "Fishable biomass SD (log10; kg/km2)"
  gmt$scale.location = "-D4.25i/0.75i/0.75i/0.075ih" # alternate: "-D4.5i/0.8i/2.5i/.25ih"
  
  gmt.map.simple(gmt)
    
  setwd( dirname( gmt$out ) ) # ps2raster has difficulties with different directories
#  cmd( "ps2raster", gmt$out, "-V -Au -P -E300 -TG -Qg2 -S" ) 
  cmd( "ps2raster", basename(gmt$out), "-V -Au -P -E300 -Tg -S" ) 






