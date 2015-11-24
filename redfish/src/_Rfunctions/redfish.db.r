redfish.db <- function(DS='logbook.redo',p=p, yrs = 2004:2015) {
	
   fn.root =  file.path( project.datadirectory("redfish"), "data", "logbook")
   dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )

 if (DS %in% c("logbook",'logbook.redo' ) {
 		if(DS=='logbook'){
				out = NULL
				fl = list.files( path=fn.root, pattern="*.rdata", full.names=T ) 
        for ( fny in fl ) {
					load (fny)
					out = rbind( out, LOGS )
				}
				return (out)
		}

#default is logbook.redo

			require(RODBC)
			con = odbcConnect(oracle.groundfish.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
			# believeNRows=F required for oracle db's

			for ( YR in yrs ) {
				fny = file.path( fn.root, paste( YR,"rdata", sep="."))
				LOGS = NULL
				LOGS = sqlQuery(con, 
									paste("select setid, vr_number_landing, vessel_name, community_name, 
											gross_tonnage, loa, gear_code, gear_desc, gear_type_id,
											gear_type_desc, landed_date, landed_form_code, year_fished,
											month_fished, to_char(date_fished, 'WW') week_fished,
											date_fished, nafo_div, unit_area, ent_latitude, ent_longitude, lat, lon,
											species_code, species, rpt_weight_kgs
													from mfd_obfmi.marfis_catch_effort
															where species_code = '120'
	                    and year_fished in (",YR,")",sep=""))
				save( LOGS, file=fny, compress=T)
				gc()  # garbage collection
				print(YR)
			}
			odbcClose(con)
			return (yrs)
		}
if(DS=='filter.region.unit3') {
		A = read.csv(find.ecomod.gis('unit3redfish'),header=T)
		A$X = as.numeric(substr(A$X,5,15))

		re = redfish.db(DS='logbook')
		re = makePBS(re,polygon=F)
		re = re[complete.cases(re[,c('X','Y','EID')]),]
		rei = findPolys(re,A,maxRows=4e5,includeBdry=1)
		re = re[which(re$EID %in% rei$EID),]
		
		return(re)		 
	}

