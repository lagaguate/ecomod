
	init.files = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 
  init.files = loadfunctions( "snowcrab", functionname="initialize.local.environment.r") 

# ----------------------------
# 1 - define plot parameters

plottimes = c("decadal",  "globalaverage")

conversions = c("ps2png", "ps2pdf")
  # choose from: conversions = c("ps2png", "ps2pdf", "ps2eps", "ps2jpg",
  #                              "eps2pdf", "png2gif", "png2mpg","png2swf")

params = NULL
params = list()

params$overlay = ""
  #    choose from:
  #    params$overlay = c("cfa20", "cfa21", "cfa22", "cfa23", "cfa24", "cfa4x",
  #      "cfa24a", "cfa24b", "cfa24c", "cfa24d", "cfa24e", "cfa23a", "cfa23b", "cfa23c", "cfa23d",
  #      "cfa22inner", "cfa22outer", "cfa20inner", "cfa20outer", "cfaslope" )

p$spatial.domain = params$spatial.domain = "SSE"
params = gmt.parameters( params ) # default settings


  set = groundfish.db( "set.complete" )
  set$sa = 1  # dummy required for mapping
  season = "summer"
  set = set[ filter.season( set$julian, period=season, index=T ) , ]


          tp = lonlat2planar( set, proj.type=p$internal.projection )
          
          tp$lon = grid.internal( tp$lon, p$lons )
          tp$lat = grid.internal( tp$lat, p$lats )
        
          tp$plon = grid.internal( tp$plon, p$plons )
          tp$plat = grid.internal( tp$plat, p$plats )
  
    tp = tp[which(tp$strat %in% 440:495),]
    tp = tp[which(is.finite(tp$plon) & is.finite(tp$plat)),]  
    
 
  Z = bathymetry.db( p=p, DS="baseline" )  # SS to a depth of 500 m  the default used for all planar SS grids
 Z = Z[which(Z$z>30),]  
  a = read.table('/home/ecomod_data/polygons/data/Science/scotia.fundy.dat',header=F)
  names(a) = c('lon','lat')
  a = lonlat2planar(a,proj.type='utm20')
  ii = which(point.in.polygon(Z$plon,Z$plat,a$plon,a$plat) !=0)
  Z = Z[ii,]
  tp$depth = tp$z
tp$z = log(tp$totwgt.cod)
tp$z[ which(is.na(tp$z))] = 0
tp1 <- tp[which(tp$yr<1985),]

geog <- gstat(id='z',formula=z~1,locations= ~plon+plat,data=tp1, nmax=20,set=list(idp=1.25))
z = predict(geog,newdata=Z)


datarange = seq( 0, max(tp1$z), length.out=150)
cols = color.code( "seis", datarange )
map( xyz=z[,c("plon", "plat", "z.pred")], depthcontours=T, pts=tp1[,c('plon','plat')], 
            at=datarange, col.regions=cols,annot.cex=6,fn='idwcod<1985')

geogam <- gam(z~s(depth,k=4)+ s(plon,plat,k=200,bs='ts'),data=tp1)
names(Z)[3] = 'depth'
Z$depth = log(Z$depth)
zgam = predict(geogam,newdata=Z,type='response',se.fit=T)
zgam = cbind(Z,zgam)

datarange = seq( 0, quantile(tp1$z,0.999), length.out=150)
cols = color.code( "seis", datarange )

map( xyz=zgam[,c("plon", "plat", "fit")], depthcontours=T, pts=tp1[,c('plon','plat')], 
            at=datarange, col.regions=cols,annot.cex=6,fn='gamdepthcod<1985')


datarange = seq( 0, quantile(zgam$se.fit,0.999), length.out=150)
cols = color.code( "seis", datarange )

map( xyz=zgam[,c("plon", "plat", "se.fit")], depthcontours=T, pts=tp1[,c('plon','plat')], 
            at=datarange, col.regions=cols,annot.cex=6,fn='gamdepthcod<1985se.fit')
