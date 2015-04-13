
	loadfunctions( "ser" ) 
  
  ns = read.table(file.path( project.datadirectory("indicators"), "regns", "regns.dat"), sep=";", header=T)
  
  years = colnames(ns)
  vars =  rownames(ns)
  
  ns = as.data.frame(t(ns))
  
  ns$yr = as.integer(gsub("X", "", years))
  t0 = min(ns$yr)
  t1 = max(ns$yr)
  
  notlog = c( "yr",
              "F - COD", "F - Mack", "F - CSole", "F - EHake", "F - EPlaic", "F - Hadd", "F - Saith", 
              "SST - S", "SST - C", "SST - N", 
              "BoT - S", "BoT - C", "BoT - N",
              "Chl - S", "Chl - C", "Chl - N",
              "Green - S", "Green - C", "Green - N",
              "Cal - S", "Cal - C", "Cal - N",
              "TCop - S", "TCop - C", "TCop - N"
              )

  log.transform = setdiff (vars, notlog)
  
  for (variable in vars) {            
    if (variable %in% log.transform) {
      q0 = ns[,variable] 
      q1 = q0[is.finite(q0)]
      minval = min(q1)
      
      out = log10( q0 )
      
      if (minval == 0) {
        q2 = q1[q1>0]
        minval2 =  min(q2)
        out = log10( q0 + minval2 )
      }
      ns[,variable] = out
    }}

  
  c = bio.form(ns, t0, t1)  
  
  # do the analysis itself
  pcadat = pca.analyse(c, yrange=c(t0:t1))


