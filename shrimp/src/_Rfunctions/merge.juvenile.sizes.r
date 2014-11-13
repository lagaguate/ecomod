
  merge.juvenile.sizes = function(year, fname, size.dir)  {

  
    # read in sizes of individuals
  sizes =shrimp.sizes( size.dir)  # R:\Shared\shrimp\data\2008\sizes make sure the last row of totals is removed 
  
  bins = seq( 10, 350, 1 )   # size ranges
  sizes$size = factor( sizes$size, levels=bins )
   
  xcount = as.data.frame.table( xtabs(  ~ set + size, data=sizes ) )
  names( xcount)= c( "set", "size", "xcount")
  xcount = factor2character( xcount, c("set", "size") )
  xcount = xcount [ which( xcount$xcount > 0 ) ,] 
  xcount$group = NA
  xcount$group [ which( as.numeric(xcount$size) >= 100 )]  = "Large" 
  xcount$group [ which( as.numeric(xcount$size) < 100 ) ] = "O-group" 

  
  ogroups = as.data.frame.table( xtabs( ~ set , data=sizes[which (sizes$group=="O-group") , ] ) )
  names( ogroups)= c( "set", "n.ogroup.sampled")
  ogroups = factor2character( ogroups, c("set") )
  
   
  # read in set info from Oracle 
    con = odbcConnect(oracle.shrimp.dsn, uid=oracle.shrimp.user, pwd=oracle.shrimp.password, believeNRows=F) 
    shrsurvey = sqlQuery(con, "select * from SHRSURVEY ")
    close(con)
    shrsurvey$year = years(shrsurvey$FDATE)

    rownumbers =  which( shrsurvey$year==year )
    shrsurvey = shrsurvey[ rownumbers , ]
    shrsurvey = shrsurvey[  , c("CRUISE","XSET","SFA") ]
    
 
  # merge set info with sizes and complete counts
  result = merge ( x=xcount, y=shrsurvey, by.x="set", by.y="XSET", all.x=T, all.y=F, sort=F) 
  
  # bring in weights  
           # R:\Shared\shrimp\data\2008\sizes
  weights = read.xls( file= fname,
      sheet=1, from=1, colNames=T, rowNames=F, type="data.frame",  stringsAsFactors=F
    )
  weights = weights[ , 1:4 ]
  
  result = merge( x=result, y=weights, by.x="set", by.y="set", all.x=T, all.y=F, sort=F)
  result = factor2character( result, c("set", "size") )

  
  result = merge( result, ogroups, by=c("set"), all.x=T, all.y=F, sort=F) 
  i.ogroup = which( result$group=="O-group")
  result$ratio = NA
  result$ratio[i.ogroup] = result$n.ogroup[i.ogroup] / result$n.ogroup.sampled[i.ogroup]

  i.large = which( result$group=="Large")
  result$ratio[i.large] = ( result$mass.large.lbs[i.large] + result$mass.xlarge.lbs[i.large] ) / result$mass.large.lbs[i.large] 
  result$carlen = as.numeric(result$size) / 10
  
  result = result[, c("CRUISE", "set", "SFA", "carlen", "xcount", "ratio")]   
  names( result) = c("bcode", "xset", "sfa", "carlen", "xcount", "ratio") 
   return (result)
   }
  
 
  
