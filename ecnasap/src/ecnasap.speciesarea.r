
# ----------------------------
#  species-area analysis

run.example = F
if (run.example) {
#  source(file.path(ecnasapdir, "load.ecnasap.environment.r"))
  
  cat = get.cat.ecnasap (source="file")
  set = ecnasap.catches(source="file")

  cat = cat[, c("setid", "spid", "totno")]
  set1 = set[, c("setid", "yr", "lon", "lat" )] 
  
  lengthscale = c( 10, 25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 400 )
  timescale = c("annual", "3yrunning", "five", "globalaverage")
  
  ntaxa = speciesarea.spatial (set1, cat, lengthscale, timescale, filtervar="totno") 
  set = cbind(set, ntaxa)

  out = "ntaxa"
  for (i in 1:length(timescale)) out = paste(out, timescale[i], sep=".")
  save(set, file=paste(out, "rdata", sep="."), compress=T)
  load(set.ntaxa)

#  st = load("ntaxa.globalaverage.rdata") .. etc..
  
 }

# --------------------------


 merge.sm.speciesarea = function (params, lengthscale) {
      
    db = params$db
    lookupregion =  params$lookupregion
    period = params$period
    region = params$region
    taxa = params$taxa
    id = params$id

    inf = dbGetQuery(gs, paste("select * from xinf"))
   
    x = dbGetQuery(gs, paste(
              'select r.id, yr, spec, r.strat, sakm2, lon, lat, julian',
              'from xset as r',
              'left join', lookupregion, ' as m on m.strat=r.strat',
              'where',
              filter.dayno.sql(period), 'and',
              filter.region.sql(region), 'and',
              filter.species.sql(taxa, id) ))
      
    v = "spec"
    fdata = speciesarea.spatial (inf, x, lengthscale, varname=v)
    g = extract.speciesarea (fdata, varname=v)
    h = merge.scaling (g, db, varname=v)
 
 }
  

# --------------------------

  merge.scaling = function(x, db, varname) {
    
    testvars =  paste(setdiff(names(x), "id"), collapse=", ")
    varsintable = dbListFields(gs, db)
    
    for (i in testvars) {
      if (i %in% varsintable) dbSendQuery(gs, paste("alter table", db, "drop", i ))
    }
   
    dbWriteTable(gs, "spsarea", x, overwrite=T, row.names=F)
    dbSendQuery(gs, "alter table spsarea change id id varchar(20)" )
    dbSendQuery(gs, "alter table spsarea add index (id) " )

    # do the merge
    dbSendQuery(gs, 'drop table if exists spsareamerge1' ) 
    dbSendQuery(gs, paste(
              'create table spsareamerge1', 
              'select i.*,', testvars,
              'from', db, 'as i ',
              'left join spsarea as n on n.id = i.id'
              ) )

    dbSendQuery(gs, paste('drop table if exists', db ) )
    dbSendQuery(gs, paste('create table', db, 'select * from spsareamerge1' ) )
    dbSendQuery(gs, paste(' alter table', db, 'change id id varchar(20)' ))
    dbSendQuery(gs, paste(' alter table', db, 'add index (id) ' ))

    dbSendQuery(gs, 'drop table if exists spsareamerge1' ) 
    dbSendQuery(gs, 'drop table if exists spsarea') 
    
  }

 # --------------------------


 extract.speciesarea = function(x, varname) {

    
    x$lsSA.radial   =  pi * (x$lengthscale ^ 2)
    
    x = x[is.finite(x$lsNtaxa) ,]
    x = x[is.finite(x$lsSA) ,]
    
    id = sort(unique(as.character(x$id)))
    nid = length(id)

    o = NULL
    nvars = 11
    o = matrix(NA, ncol=nvars, nrow=nid)

    for (i in 1:nid) {
      print (paste(i, "of", nid))
      y = x[x$id == id[i] ,]

      if (dim(y)[1] > 5) {
#      plot(y$lsSA, y$lsNtaxa)      
#      lines(y$lsSA.radial, y$lsNtaxa)      
#      rln = lm( log.ntaxa ~ log.saLs, data=y)
#      rlns = summary(rln)
      
#      mod0  = summary(nls(log(lsNtaxa) ~ log(c) + z * log(lsSA), data=y, start=list(c=2, z=0.25)))$parameters
#      mod1  = summary(nls(log(lsNtaxa) ~ log(c) + z * log(lsSA.radial), data=y, start=list(c=2, z=0.25)))$parameters
       
      log.ntaxa = log(y$lsNtaxa)
      log.sa = log(y$lsSA)
      log.sa.radial = log(y$lsSA.radial)

      lm0 = lm( log.ntaxa ~ log.sa)
      mod0 = summary(lm0)$coefficients

      lm1 = lm( log.ntaxa ~ log.sa.radial)
      mod1 = summary(lm1)$coefficients

      sa0 = 2e5  # approx size of ss
      sa1 = sa0
      
      p0 = exp(mod0[1,1] + mod0[2,1]*log(sa0))
      p1 = exp(mod1[1,1] + mod1[2,1]*log(sa1))
      
      o[i,] = c( id[i], mod0[,1], mod0[,2], mod1[,1], mod1[,2], p0, p1)
      }
    }

    o = as.data.frame(o)
    names(o) = c( "id", paste(varname, c("creal", "zreal", "crealse", "zrealse", 
                                         "cradial", "zradial", "cradialse", "zradialse",
                                         "preal", "pradial" ), sep=""))

    chars = c("id")
    numbers = setdiff(names(o), chars)

    for (i in names(o)) o[,i] = as.character(o[,i])
    for (i in numbers) o[,i] = as.numeric(o[,i])

    o = o[!is.na(o$id) ,]

    dbWriteTable(gs, "paramsspeciesarea", o, overwrite=T, row.names=F)
 
      
  return(o)

  }


# --------------------------

  speciesarea.spatial = function (inf, x, lengthscale, varname) {

    source(file.path(griddir, "geodesy.r"))
 
    id = sort(unique(as.character(x$id)))
   nid = length(id)

    nvars = 8
    out = NULL

    coords = c("lon", "lat")

    for (k in lengthscale) {
      o = NULL
      o = matrix(NA, ncol=nvars, nrow=nid)
      for (i in 1:nid) {

        print (paste(k,":", i, "of", nid))

        p.id = id[i]
        p.inf = which(inf$id==p.id)
        p.year = as.numeric(substr(p.id,4,7))

        q.inf = which(geodist(point=inf[p.inf, coords], locations=inf[coords], method="great.circle") <= k)
        q.id = sort(unique(as.character(inf$id[q.inf])))
        q.xinf = which(x$id %in% q.id & x$yr==p.year)

        d = x[q.xinf, varname]
        d = d[which(is.finite(d))]

        ntaxa = length(unique(d))
        sa = sum(inf$sakm2[q.inf])
 
        o[i,] = c( p.id, as.matrix(inf[p.inf,coords]), p.year, k, varname, sa, ntaxa )

      }

    out = rbind(out, as.data.frame(o) )

    }

    names(out) = c( "id", coords, "yr", "lengthscale", "varname", "lsSA", "lsNtaxa" )

      chars = c("id", "varname")
      numbers = setdiff(names(out), chars)

    for (i in names(out)) out[,i] = as.character(out[,i])
    for (i in numbers) out[,i] = as.numeric(out[,i])

    dbWriteTable(gs, "speciesarea", out, overwrite=T, row.names=F)
  

    return(out)
  }




  # -------------------------------
  #	resample species richness randomly
  # -------------------------------

  resample.richness.random = function (inf, x, niterations, nsetspossible) {

    id = sort(unique(as.character(x$id)))
  	nid = length(id)
	  idrange = c(1:nid)

    o = matrix(NA, ncol=10, nrow=niterations)

    for (i in 1:niterations) {
    	nsubsample = floor(runif(1)*nsetspossible)+1
      resampled = id[sort(as.numeric(as.character(sample(idrange, nsubsample, replace=T))))]
      locs = sort(unique(as.numeric(as.character(resampled))))
      k = which(inf$id %in% locs)
      sa = sum(inf$sakm2[k])

      d = x[x$id %in% locs , ]
      ntaxa = length(unique(d$spec))
      btot = sum(d$totwgt, na.rm=T)
      depth = exp(mean(log(d$sdepth[is.finite(d$sdepth)])))
      depth.sd = exp(sd(log(d$sdepth[is.finite(d$sdepth)])))
      dt = d$temp[is.finite(d$temp)]
      temp = mean(dt, na.rm=T )
      temp.sd = sd(dt, na.rm=T)
      sal = exp(mean(log(d$sal[is.finite(d$sal)])))
      sal.sd = exp(sd(log(d$sal[is.finite(d$sal)])))
      julian = mean(d$julian, na.rm=T)

      o[i,] = c(sa, ntaxa, btot, depth, depth.sd, temp, temp.sd, sal, sal.sd, julian)
    }

    o=as.data.frame(o)
    names(o) = c("sa", "ntaxa", "btot", "depth", "depth.sd", "temp", "temp.sd", "sal", "sal.sd", "julian")

#    x11()
#   plot(log(o$sa), log(o$ntaxa), col="blue")
    r = lm(log(o$ntaxa) ~ log(o$sa)+o$depth + o$temp + o$sal + o$btot)
    summary(r)
#
#

    return(o)
  }



  # -------------------------------
  #	resample species richness with spatial constraints
  # -------------------------------

  # lengthscale = c(10, 20, 40, 80, 160, 320, 640, 1280, 2560)
  # niterations = 5000


  breakdown.by.year = function (x, vars) {

    yrs = sort(unique(as.numeric(as.character(x$yr))))
    nyrs = length(yrs)
    out = matrix(NA, ncol=4, nrow=nyrs)

    for (y in 1:nyrs) {
      d = x[which(x$yr == yrs[y]), vars]
      meanx = mean(d, na.rm=T)
      varx  = var(d, na.rm=T)
      nx = length(d)
      out[y,] = c(yrs[y], meanx, varx, nx)
    }

    out=as.data.frame(out)
    names(out) = c("yr", "m", "v", "n")

    return (out)

  }

 # -------------------------------
  

  resample.richness.spatial = function (inf, x, niterations=1, lengthscale) {

    id = sort(unique(as.character(x$id)))
  	nid = length(id)
	  idrange = c(1:nid)

    nvars = 10
    out = NULL

    coords = c("lon", "lat")
    for (k in lengthscale) {
      o = NULL
      o = matrix(NA, ncol=nvars, nrow=nid)
		  for (i in 1:nid) {
			  p.id = id[i]
        p.inf = which(inf$id==p.id)
        p.year = as.numeric(substr(p.id,4,7))

        q.inf = which(geodist(point=inf[p.inf, coords], locations=inf[coords], method="vincenty") <= k)
        q.id = sort(unique(as.character(inf$id[q.inf])))
        q.xinf = which(x$id %in% q.id & x$yr==p.year)

        d = x[q.xinf,]

        sa = sum(inf$sakm2[q.inf])
        ntaxa = length(unique(d$spec[d$id %in% inf$id[q.inf]]))

  	 	  bvar = var(d$totwgt, na.rm=T)
        o[i,] = c( p.id, as.matrix(inf[p.inf,coords]), p.year, k, sa,
                   ntaxa, sum(inf$totwgt[q.inf]), mean(d$temp , na.rm=T), mean(d$sdepth, na.rm=T)
                 )
      }
      out = rbind(out, as.data.frame(o) )
    }

    names(out) = c( "id", coords, "yr", "lengthscale", "sa",
                    "richness", "btot", "temp", "depth" )
    chars = c("id")
    numbers = c(coords, "yr", "lengthscale", "sa", "richness", "btot", "temp", "depth" )

    for (i in names(out)) out[,i] = as.character(out[,i])
    for (i in numbers) out[,i] = as.numeric(out[,i])

    return(out)
  }


  raw.richness = function (inf, x) {

    id = sort(unique(as.character(x$id)))
  	nid = length(id)
	  idrange = c(1:nid)

    nvars = 12
    o = matrix(NA, ncol=nvars, nrow=nid)

		for (i in 1:nid) {
			p.id = id[i]
      p.inf = which(inf$id==p.id)
      p.x = which(x$id== p.id)
      d = x[p.x,]

      sa = inf$sakm2[p.inf]
      ntaxa = length(unique(d$spec[d$id == p.id]))
      btot = sum(d$totwgt, na.rm=T)
      ntot = sum(d$totno, na.rm=T)
      temp = unique(d$temp)[1]
      depth = unique(d$sdepth)[1]
      salinity = unique(d$sal)[1]
      yr = unique(d$yr)[1]
      julian = unique(d$julian)[1]

 	    o[i,] = c( p.id, as.matrix(inf[point,coords]), yr, julian, sa, ntaxa, btot, ntot, temp, depth, salinity )
    }

    out = as.data.frame(o)


    names(out) = c("id", coords, "yr", "julian", "sa", "ntaxa", "btot", "ntot", "temp", "depth", "salinity" )
    chars = c("id")
    numbers = c(coords, "yr", "julian", "sa", "ntaxa", "btot", "ntot", "temp", "depth", "salinity" )

    for (i in names(out)) out[,i] = as.character(out[,i])
    for (i in numbers) out[,i] = as.numeric(out[,i])


    x11()
#    plot(out$sa, out$richness, col="blue")

    return(out)

  }




