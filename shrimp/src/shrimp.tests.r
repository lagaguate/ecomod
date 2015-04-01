

	loadfunctions( "shrimp", functionname="load.shrimp.environment.r" )

  
# --------------------------
# minilog data cleaning ...
# data file
  datapath = file.path( project.datadirectory("shrimp"), "data","miniA" )
  outputA = extracts.minilog.stats( datapath )

  datapath = file.path( project.datadirectory("shrimp"), "data","miniB" )
  outputB = extracts.minilog.stats( datapath )

  dat.merged = rbind( outputA, outputB )
  dat.merged = dat.merged[ order( dat.merged$t0 ), ]

  write.table(dat.merged,"clipboard",sep="\t",col.names=NA)



# --------------------------
# This section works on the size data and merges into a loadable format
      
  result = merge.juvenile.sizes (
    year = 2008, 
    fname=file.path( project.datadirectory("shrimp"), "data", "2008", "weights", "weights.xls") 
    size.dir=file.path( project.datadirectory("shrimp"), "data", "2008", "sizes" ) 
  ) 
 
#  write.table( result, "clipboard", sep="\t",col.names=T)
  write.table( result, file=file.path(project.datadirectory("shrimp"), "d:q
  ata", "2008", "test.csv"), sep="\t", col.names=T)
                                                  
  
    
  
