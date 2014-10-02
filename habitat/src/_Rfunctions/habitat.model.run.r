  
  habitat.model.run = function( ww, pdat, formu, optimizer ) {
            
    fmly = habitat.model.family.lookup(ww)
    
    mfunc = function( ww, pdat, fmly) { bam( formu, data=pdat, na.action="na.omit", family=fmly )}
    models =  try ( mfunc(ww, pdat, fmly) )
    
    if  ( "try-error" %in% class(models) ) { 
      mfunc = function(ww, pdat, fmly) { gam( formu, data=pdat, optimizer=optimizer, na.action="na.omit", family=fmly )}
      models =  try ( mfunc(ww, pdat, fmly) )
    }
    
    if  ( "try-error" %in% class(models) ) { 
      mfunc = function(ww, pdat, fmly) { gam( formu, data=pdat, na.action="na.omit", family=fmly )}
      models =  try ( mfunc(ww, pdat, fmly) )
    }
    
    if  ( "try-error" %in% class(models) ) models="model.failure"
    
    return (models)
  }


