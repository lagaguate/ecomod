
  extract.3d.field = function( archive, params, nt, save.states, type ) {

    Q =  array( NA, dim=c( params["ilo"], params["m"], params["n"] ) )

    fn = paste( archive, type, sep ="." )
    fn3 = file(fn, "rb", encoding="UTF-8")

    for ( layer in 1: save.states["savelayer"] ) {
      readBin( fn3, what="raw", n=4 )
      p3 = readBin( fn3, what="integer", n=9 ) # "kstep_u", "sec", "min", "hour", "day", "month", "year" 31152
      names(p3) = c("kstep_u",  "sec", "min", "hour", "day", "month", "year", "ncomp", "layer" )
      readBin( fn3, what="raw", n=16 )
      
   #   readBin( fn3, what="raw", n=4 )
      packed = readBin( fn3, what="double", n=p3["ncomp"] )
      readBin( fn3, what="raw", n=4 )

      O = invcompact3( params, layer, K=1, packed )  # K is the scale factor
      Q[layer,,] = O
    }
    close( fn3 )
    return (Q)
  }


