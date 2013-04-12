  

  condition.estimation.model = function( DS="saved" ) {
      
    fn_model = ""
    
    if (DS=="saved") {

      return( x )
    }

    SS = bio.db( "set" ) 
    DD = bio.db( "det" ) 

    x = merge (DD, SS)

    # lwmodel = glm( log(mass) ~ log(len) + sex + spec + temp + space + depth .... full model ,  
           
    # then compute deviations from predicted mass and 
    # then interpolate 

    return(x)
  }
      
  
