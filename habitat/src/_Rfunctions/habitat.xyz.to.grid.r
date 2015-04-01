
  habitat.xyz.to.grid = function( p, redo=FALSE ) {

    ddir = project.datadirectory( "habitat" , "data" )

    fn = file.path( ddir, paste( "habitat.xyz2grid", p$spatial.domain, "rdata", sep="." ) ) 
 
    if (redo) {
    
      loadfunctions("bathymetry")  # base structure from baythmetry db
      H = bathymetry.db( p=p, DS="baseline" )  
      row = round(( H$plon - min(H$plon) ) / p$pres ) + 1
      col = round(( H$plat - min(H$plat) ) / p$pres ) + 1
      row.col = cbind(row, col)
      save ( row.col, file=fn, compress=TRUE )

    } else {

      load(fn) 

    }
      
    return(row.col) 
  }
  

