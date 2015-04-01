
  get.time.series = function(x=NULL, regions=NULL, vars=NULL, trim=0, from.file=F, outfile=NULL,reduced.stations=F) {

    if (is.null(outfile) & !reduced.stations) outfile = file.path( project.datadirectory("snowcrab"), "R", "ts.rdata" )
    if (is.null(outfile) & reduced.stations) outfile = file.path( project.datadirectory("snowcrab"), "R", "ts.reduced.rdata" )

    ts= NULL

    if( from.file) {
      if (file.exists(outfile) ) load(outfile)
      return(ts)
    }
   
    # ra.jit(2)  # JIT optimisation
 if(reduced.stations) {
    b = x[which(x$yr==2014),'station']
    x = x[which(x$station %in% b),]
    }
      out = NULL
      tmpfile = file.path( tempdir(), make.random.string(".ts.csv"))

      for (r in regions) {
        print (r)
        qr = filter.region.polygon(x, r)
        for (yrs in unique(x$yr)) {
          qy = which( x$yr == yrs )
          filter = sort(unique(intersect(qr, qy)))
          if (length(filter)>0) {
            for (v in vars) {
              z = length( which( is.finite( x[ filter, v] ) ) )
              if (z < 1) next()
              y = x[ filter, ]
              q = variable.recode( y[,v], v, direction="forward", db="snowcrab" ) # transform variables where necessary
              m =  mean (q, trim=trim, na.rm=T)
              n = length(q)
              se = sd(q, na.rm=T)/ sqrt(n-1)
              meanval = variable.recode (m , v,  direction="backward", db="snowcrab" )
              ub = variable.recode ( m+se*1.96, v,  direction="backward", db="snowcrab" )
              lb = variable.recode ( m-se*1.96, v,  direction="backward", db="snowcrab" )
              j = as.data.frame(cbind(r, yrs, v, meanval, se, ub, lb, n))
              write.table(j, file=tmpfile, append=T, row.names=F, col.names=F, quote=F, sep=";")
            }
          }
      }}

      ts = NULL
      if (file.exists( tmpfile)) {
        ts = read.table(tmpfile, sep=";", as.is=T, colClasses="character", header=F)
        colnames(ts) = c("region", "year", "variable", "mean", "se", "ub", "lb", "n")
        numbers = c("year", "mean", "se", "n", "ub", "lb")
        ts = factor2number(ts, numbers)
        save(ts, file=outfile, compress=T)
      }
      remove.files(tmpfile)
     return(ts)

  }


