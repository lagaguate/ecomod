
  jags.setup = function( jags.model ) {
    jags.outfile = tempfile()
    cat(jags.model, file=jags.outfile)
    return( jags.outfile ) # return the file name    
  }

        

