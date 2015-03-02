 
  map = function( xyz, cfa.regions=T, depthcontours=T, pts=NULL, colpts=F, annot=NULL, annot.cex=2.2, 
                 leg = NULL, projection = "utm20", col.regions=F, at=0:1, 
                 fn=paste("map", trunc(runif(1)*1e8), sep=""), loc=tempdir(), 
                 corners=NULL, rez=c(1,1), spatial.domain="canada.east", ... ) {
   
    # map using levelplot ... no GMT dependency
		
		require( lattice )

    xlim =ylim = NULL
    colorkey=list(space="right", labels=list(cex=3)) # these are lattice options

    if (ncol( xyz) == 2) { # assuming points
      xyz = cbind( xyz, 1)
      colorkey=F
    }

    if ( is.null( xyz$plon) ) {
      names( xyz ) = c( "lon", "lat", "z" )
      xyz = lonlat2planar( xyz,   proj.type=projection)
      xyz = xyz[ , c( "plon", "plat", "z" ) ]
      if ( !is.null(corners) ) {
        if ( is.null(corners$plon) ) corners = lonlat2planar( corners,  proj.type= projection )
      }
      
    } else {
      names( xyz ) = c( "plon", "plat", "z" )
    } 

    if ( is.logical( colorkey) )  pts = xyz      # flag from above when only XY data are passed,.. after conversion to planar coords
    
    if ( !is.null(corners) ) {
      xlim = range( corners$plon)
      ylim = range( corners$plat)
    } else {
      xlim = range( xyz$plon )
      ylim = range( xyz$plat )
    }
      
    if ( ! is.null(pts) ) { 
      if ( is.null( pts$plon) ) {
        pts = lonlat2planar(xyz,  proj.type="utm20") 
        pts = pts[, c("plon", "plat")]
      }
    }
		
		pp = list( spatial.domain=spatial.domain )

    lp = levelplot( z ~ plon+plat, data=xyz, aspect="iso", pts=pts, colpts=colpts, annot=annot, pp=pp, 
      annot.cex=annot.cex, xlab="", ylab="", scales=list(draw=F), col.regions=col.regions, at=at, xlim=xlim, ylim=ylim, 
      colorkey=colorkey , rez=rez, leg=leg,  cfa.regions=cfa.regions,
      panel = function(x, y, subscripts, rez=rez,  ...) {
        
        panel.levelplot (x, y, subscripts, aspect="iso", rez=rez, ...)
    
        if ( !is.null(pts) ) { 
          if (colpts) {  # overlay above with larger sized points, esp if there is colour
            colb = findInterval( z, at)
            for ( ii in 1:length(z) ) {
              panel.xyplot( pts$plon[ii], pts$plat[ii],  aspect="iso", col=col.regions[colb[ii]],
                  panel = panel.rect, height = rez[1], width = rez[2], ... ) 
            }
          } else {
              panel.xyplot( pts$plon, pts$plat,  aspect="iso", col = "black", pch=16, 
                  panel = panel.rect, height = rez[1], width = rez[2], ... ) 
          }
        }

        if (depthcontours) {
          zc = isobath.db( p=pp, depths=c(100, 300,  500, 700 ) )  
          zc = lonlat2planar( zc, proj.type= projection )
          panel.xyplot( zc$plon, zc$plat, col = "darkgrey", pch=".", cex=0.6 )

          zc = isobath.db( p=pp, depths=c(200, 400, 600 ) )  
          zc = lonlat2planar( zc, proj.type= projection )
          panel.xyplot( zc$plon, zc$plat, col = "grey", pch=".", cex=0.6 )
        }

        if ( cfa.regions ) {

          # coords of boundaries .. to be moved to polygon database ...
          cfa.nens.23 = data.frame( rbind( 
            c(-59.85, 46), 
            c(-58.40, 46)
          ))
          cfa.23.24 = data.frame( rbind( 
            c(-59.065, 43.5), 
            c(-59.959007, 44.829624), 
            c(-60.51667, 45.61667)
          ))
          cfa.4x.24 = data.frame( rbind( 
            c( -63.333333, 42.61379),
            c( -63.333333,	44.332904),
            c( -63.50242, 44.502358)
          ))
          
          names( cfa.nens.23 ) = names( cfa.23.24 ) = names( cfa.4x.24 ) = c("lon", "lat")

          cfa.nens.23 = lonlat2planar( cfa.nens.23, proj.type= "utm20") 
          cfa.23.24 = lonlat2planar( cfa.23.24, proj.type= "utm20") 
          cfa.4x.24 = lonlat2planar( cfa.4x.24,  proj.type="utm20") 
       
          panel.lines( cfa.nens.23$plon, cfa.nens.23$plat, col = "darkgray", lwd=2 )
          panel.lines( cfa.23.24$plon, cfa.23.24$plat, col = "darkgray", lwd=2 )
          panel.lines( cfa.4x.24$plon, cfa.4x.24$plat, col = "darkgray", lwd=2 )
        
        }
                  
        #coastline
        zc = isobath.db( p=pp, depths=c(0)  ) 
        zc = lonlat2planar( zc, proj.type= projection )
        panel.xyplot( zc$plon, zc$plat, col = "black", pch=".", cex=1 )

        if (is.null(leg) ) {
				  xoffset = 30
				  leg = c( xlim[2]-xoffset, ylim[1] + 0.2*(ylim[2]-ylim[1]) ) 
        }

        panel.arrows( x0=leg[1]-100, y0=leg[2], x1=leg[1], y1=leg[2], 
						angle=90, length=0.06, ends="both", lwd=3, col="black", ...)
        panel.text( x=leg[1]+18, y=leg[2]+25, "100 km", cex=1.7, pos=2 )
        
        if ( !is.null( annot ) ){
          panel.text( x=leg[1] + 25, y=ylim[1] + 0.12*(ylim[2]-ylim[1]), annot, cex=2, pos=2 )  # pos=2 is left of (right justified)
        }
    } # end panel
    ) # end levelplot

    dir.create (loc, showWarnings=FALSE, recursive =TRUE) 
    fn = file.path( loc, paste(fn, "png", sep="." ) )
    png(  filename=fn, width=3072, height=2304, pointsize=40, res=300 ) 
    print(lp)
    dev.off()
   
    return( fn )

  }


