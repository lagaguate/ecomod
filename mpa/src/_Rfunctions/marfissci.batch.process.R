marfissci.batch.process <- function(df, agg.by="SPECIES_CODE"){
  combos = unique(df[agg.by])
  for (i in 1:nrow(combos)){
#    print(combos[i,])
#    print(nrow(df[df$SPECIES_CODE==combos[i,],]))
    marfissci.process.data(df[df[agg.by]==combos[i,],])
  }
}


files <- list.files(path=paste0(project.datadirectory("mpa"),"/RDS/"), pattern = "\\.rds$")
for (f in 1:length(files)){
  marfissci.simple.map(readRDS(paste0(project.datadirectory("mpa"),"/RDS/",files[f])))
}