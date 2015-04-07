#depths
#C:\cygwin64\home\choij\ecomod\biochem\data\datadump

biochem.datadump.dir = file.path( "C:","cygwin64","home","choij", "ecomod", "biochem", "data", "datadump" ) 
fn=file.path(biochem.datadump.dir,"chl.data.dump.odbc.all_years.rdata")
load(fn)

names(out)=tolower(names(out))
ncf=out

ncf$mission=as.character(ncf$mission)

um=unique(ncf$mission)

df=NULL
tmp=NULL
for (j in 1:length(um)) {
  
  mid=which(ncf$mission==um[j])

for (i in 1:length(ncf$collector_event_id[mid])) {
  
  tmp$mission=um[j]
  tmp$mission_date=ncf$start_date[mid[1]]
  tmp$event=ncf$collector_event_id[i]
  tmp$number_of_depths=length(unique(ncf$header_start_depth[i]))
  tmp$min_depth=min(unique(ncf$header_start_depth[i]))
  tmp$max_depth=max(unique(ncf$header_start_depth[i]))
  
  df=rbind(df,tmp)
  

}

}


fn=file.path("C:","Gordana","depth.csv")
df=read.csv(fn)

