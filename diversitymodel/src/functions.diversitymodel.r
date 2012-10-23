
    
  diversity.model.jags = function( DS="" ) {
    out = NULL
    
    if (DS=="logistic" ) {
      out = file.path( project.directory("diversity")model, "src", "logistic.bugs" )    
    }
    return(out)
  }


  count.richness = function( x, DS="simple") {
    
    if (DS %in% c( "simple", "simple.redo") ) {
      fn = file.path( project.directory("diversity")model, "data", "simple.rdata" ) 
      if (DS=="simple") {
        if (file.exists( fn)) load(fn)
        return (cnts)
      }
    

      

    }
  
  }




