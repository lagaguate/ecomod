
  jags.setup = function( jags.model, workdir = "/tmp" ) {
    jags.outfile = file.path( workdir, paste( "jags.tmp", runif(1), sep="") )
    cat(jags.model, file=jags.outfile)
    return( jags.outfile ) # return the file name    
  }

        

