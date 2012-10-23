 
  make.maps.core = function( id=NULL, U, params, variables, plottimes, basedir, conversions, delta, init.files, db ) {
   
    if (!is.null( init.files ) ) { for (ii in init.files) source(ii) }
    varnames = colnames(U)
     
    # the first index is a list that is passed from the calling prog: in this case "ssplt" (if parallel)
    if (params$do.parallel) id = as.numeric(id)
    if (!params$do.parallel) id = c(1:length(variables) )
    
    for (i in id) {
      plotvar = variables[i]
      if (! (plotvar %in% varnames) ) next
      for (ti in plottimes) {
        u = recode.time(U, ti, delta )  # delta is for running averages
        u[,plotvar] = variable.recode( u[,plotvar], plotvar, "forward", db=db) # check if log transform is needed
        u = u[, c("yr", "lon", "lat", plotvar, "sa")]
        u = u[is.finite(u[,plotvar] *u[,"lon"]*u[,"lat"] ),]
        params$outdir = file.path(basedir, ti, plotvar)
        params = gmt.resolution(params)
        params = gmt.projection(params)
        params = gmt.defineregion(params)
        params = gmt.define.colours (params, variable=plotvar)
        params = gmt.colourscale(params, u[,plotvar], plotvar, NSTD=3 ) # NSTD is no of stdev
        
        dir.create ( params$outdir, recursive=T, showWarnings=F )
                
        for (i in sort(unique(u$yr)) ) {
          toplot = u[which(u$yr==i),]
          if (!is.null(dim)) {
            toplot = toplot[,  c("lon", "lat", plotvar, "sa") ]
            toplot = toplot[is.finite(toplot[,1]*toplot[,2]*toplot[,3] ),]
            if (dim(toplot)[1]>0) {
              params$outfile.basename = file.path(params$outdir, paste(plotvar, i, sep="."))
              # print( params$outfile.basename )
              smoothed = gmt.map( params, toplot, year=i, vname=plotvar, conversions=conversions )
            }
          }
        }
    }}
    return (params)
  }


