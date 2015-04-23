#######################################################################################
#######################################################################################
#######################################################################################
###                                                                                 ###
###       Title:                Observer Program Code Tables to R                   ###
###                                                                                 ###
###       Author:               Peter Comeau                                        ###
###                                                                                 ###
###       Creation Date:        December 8, 2014                                    ###
###                                                                                 ###
###       Modification Date:    February 11, 2015                                   ###
###                                                                                 ###
###       Description:          Creates a List of Observer Code Tables              ###
###                             and saves them to an Rdata file.                    ###
###                                                                                 ###
###                                                                                 ###
###                                                                                 ###
###                                                                                 ###
###                                                                                 ###
#######################################################################################
#######################################################################################
#######################################################################################

#######################################################################################
###                          PACKAGES                                               ###
#######################################################################################

library(RODBC)
library(reshape)
library(lubridate)

#######################################################################################
###                          DIRECTORY PATHS                                        ###
#######################################################################################

tm.start <- as.POSIXct(now())

data.path <- "C:\\Users\\comeaupa\\Documents\\My Documents\\r-data\\"
code.path <- "C:\\Users\\comeaupa\\Documents\\My Documents\\r-code\\"
output.path <- "C:\\Users\\comeaupa\\Documents\\My Documents\\r-output\\"
resources.path <- "C:\\Users\\comeaupa\\Documents\\My Documents\\r-resources\\"





#######################################################################################
###                          MULTI-PAGE GRAPHICS                                    ###
#######################################################################################


graphics.off()
if(exists(".SavedPlots",where=1)==T){rm(.SavedPlots,pos=1)}
windows(record=T); 


#######################################################################################
###                          DATABASE CONNECTIONS                                   ###
#######################################################################################


source(paste(code.path,"connections.r",sep=""))

#######################################################################################
###                          CONSTANTS                                              ###
#######################################################################################

years <- 1977:2014
tripcd.group.id <- c('groundfish', 'pelagics', 'invertebrates', 'research', 'lobster')
tripcd.group <- c('tripcd_id in (12, 14, 23, 30, 31, 49, 122, 211, 220, 241, 312, 400, 414, 7001, 7002, 7099)',
                  'tripcd_id in (60, 70, 72, 73, 230)', 
                  'tripcd_id in (2210, 2509, 2550, 4211, 4320, 4355, 4511, 6400, 6600)',
                   'tripcd_id in (7000, 7010, 7011, 7050, 7051, 7052, 7053, 7054, 7055, 7057, 7058, 7059, 7060, 7061, 7062, 7063, 7064, 7065)',
                   'tripcd_id in (7051, 7065)')


#######################################################################################
###                          FUNCTIONS                                              ###
#######################################################################################



for (i in 1:length(tripcd.group)) {
       for (j in 1977:2014) {

       
#1  istrips
       
		trips <- paste("select * from istrips where ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#2   isfishsets
		
				sets <- paste("select s.fishset_id, s.trip_id, s.set_no, s.setcd_id, s.gear_id, s.specscd_id, s.station, s.stratum_id, 
		               s.hydcd_id, s.est_catch, s.comarea_id, s.resarea_id, s.eezarea_id, s.nafarea_id, s.source, s.haulccd_id, 
		               s.len_longline, s.num_hook_haul, s.comments, s.last_update_by, s.last_update_date 
		               from isfishsets s, istrips t 
		               where t.trip_id = s.trip_id 
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")
       
#3  issetprofile

				setprofile <- paste("select p.setprof_id, p.set_no, p.setdate, p.settime, p.pntcd_id, p.botcd_id, p.latitude, p.latddmm, 
				                            p.longitude, p.longddmm, p.depth, p.air_temperature, p.net_temperature, p.water_temperature, 
				                            p.bar_pressure, p.vessel_speed, p.comments, p.last_update_by, p.last_update_date  
		               from issetprofile p, isfishsets s, istrips t 
		               where t.trip_id = s.trip_id 
		               and p.fishset_id = s.fishset_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#4  isgears

				gears <- paste("select g.gear_id, g.trip_id, g.gear_no, g.gearcd_id, g.wingspread, g.body_mesh_size, g.hookcd_id, g.hooksize, 
				                       g.last_update_by, g.last_update_date 
		               from isgears g, istrips t 
		               where t.trip_id = g.trip_id 
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#5  isgearfeatures

				gearfeatures <- paste("select gf.gear_id, gf.gearfcd_id, gf.gear_id, gf.feature_value, gf.last_update_by, gf.last_update_date 
		               from isgearfeatures gf, isgears g, istrips t 
		               where g.trip_id = t.trip_id
		               and gf.gear_id = g.gear_id 
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")
       
       
#6  iscatches
		               
		catches <- paste("select c.catch_id, c.fishset_id, c.speccd_id, c.set_no, c.est_num_caught, c.est_kept_wt,
		                  c.est_discard_wt, c.est_reduction_wt, c.est_combined_wt, c.last_update_by, c.last_update_date 
		               from iscatches c, isfishsets s, istrips t 
		               where t.trip_id = s.trip_id 
		               and s.fishset_id = c.fishset_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")               

       
#7  issamples

				samples <- paste("select sa.smpl_id, sa.catch_id, sa.sexcd_id, sa.prodcd_id, sa.species_sample_weight, sa.sexed_sample_weight, 
				                         sa.sex_weight_code, sa.sub_sample_flag, sa.last_update_by, sa.last_update_date 
		               from issamples sa, iscatches c, isfishsets s, istrips t 
		               where s.trip_id = t.trip_id 
		               and c.fishset_id = s.fishset_id
		               and sa.catch_id = c.catch_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#8  isfishlengths

				fishlengths <- paste("select l.fishlen_id, l.smpl_id, l.fish_length, l.num_at_length, l.sex_weight_code, l.species_sample_weight,
				                             l.sexed_sample_weight, l.last_update_by, l.last_update_date 
		               from isfishlengths l, issamples sa, iscatches c, isfishsets s, istrips t 
		               where s.trip_id = t.trip_id 
		               and c.fishset_id = s.fishset_id
		               and sa.catch_id = c.catch_id
		               and l.smpl_id = sa.smpl_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#9  isfish

				fish <- paste("select f.fish_id, f.catch_id, f.fish_no, f.sexcd_id, f.fish_length, f.fish_weight, f.matcd_id,
				                      f.fish_age, f.agercd_id, f.otolith_collected, f.comments, f.last_update_by, f.last_update_date 
		               from isfish f, iscatches c, isfishsets s, istrips t 
		               where s.trip_id = t.trip_id 
		               and c.fishset_id = s.fishset_id
		               and f.catch_id = c.catch_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#10  isfishmorphs

				fishmorphs <- paste("select fm.fish_id, fm.mrphcd_id, fm.fishmorph_id, fm.mrphvcd_id, fm.quant_value,
				                            fm.last_update_by, fm.last_update_date 
		               from isfishmorphs fm, isfish f, iscatches c, isfishsets s, istrips t 
		               where s.trip_id = t.trip_id 
		               and c.fishset_id = s.fishset_id
		               and f.catch_id = c.catch_id
		               and fm.fish_id = f.fish_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

       
#11  isfishstmanalysis

				fishstmanalysis <- paste("select fs.fish_id, fs.analysis_no, fs.date_analyzed, fs.techcd_id, fs.stm_total_wt,
				                                 fs.stm_empty_wt, fs.stm_fullness, fs.last_update_by, fs.last_update_date 
		               from isfishstmanalysis fs, isfish f, iscatches c, isfishsets s, istrips t 
		               where s.trip_id = t.trip_id 
		               and c.fishset_id = s.fishset_id
		               and f.catch_id = c.catch_id
		               and fs.fish_id = f.fish_id
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")

		                      
#12  isfishprey

				fishprey <- paste("select fp.fishpry_id, fp.prey_no, fp.stmitcd_id, fp.stmccd_id,
				                          fp.quantity, fp.avg_length, fp.avg_weight, fp.comments, fp.last_update_by, fp.last_update_date, fp.fishstmanalysis_id 
		               from isfishprey fp, isfishstmanalysis fs, isfish f, iscatches c, isfishsets s, istrips t 
		               where s.trip_id = t.trip_id 
		               and c.fishset_id = s.fishset_id
		               and f.catch_id = c.catch_id
		               and fs.fish_id = f.fish_id
		               and fp.fishstmanalysis_id = fs.analysis_no
		               and ",tripcd.group[i]," and to_char(board_date, 'YYYY') = ", j, ";",sep = "")
		               
		               		
		istrips <- sqlQuery(RODBC_observer,trips)
		issets <- sqlQuery(RODBC_observer,sets)
		issetprofile <- sqlQuery(RODBC_observer,setprofile)
		isgears <- sqlQuery(RODBC_observer,gears)
		isgearfeatures <- sqlQuery(RODBC_observer,gearfeatures)
		iscatches <- sqlQuery(RODBC_observer,catches)
		issamples <- sqlQuery(RODBC_observer,samples)
		isfish <- sqlQuery(RODBC_observer,fish)
		isfishlengths <- sqlQuery(RODBC_observer,fishlengths)
		isfishmorphs <- sqlQuery(RODBC_observer,fishmorphs)
		isfishstmanalysis <- sqlQuery(RODBC_observer,fishstmanalysis)
		isfishprey <- sqlQuery(RODBC_observer,fishprey)
		
		
		
#		assign(paste("trips",tripcd.group.id[i], j, sep = "."), istrips)
#		assign(paste("sets",tripcd.group.id[i], j, sep = "."), issets)
#	    assign(paste("setprofile",tripcd.group.id[i], j, sep = "."), issetprofile)
#	    assign(paste("gears",tripcd.group.id[i], j, sep = "."), isgears)
#	    assign(paste("gearfeatures",tripcd.group.id[i], j, sep = "."), isgearfeatures)
#	    assign(paste("catches",tripcd.group.id[i], j, sep = "."), iscatches)
#	    assign(paste("samples",tripcd.group.id[i], j, sep = "."), issamples)
#	    assign(paste("fish",tripcd.group.id[i], j, sep = "."), isfish)
#	    assign(paste("fishlengths",tripcd.group.id[i], j, sep = "."), isfishlengths)
#	    assign(paste("fishmorphs",tripcd.group.id[i], j, sep = "."), isfishmorphs)
#	    assign(paste("fishstmanalysis",tripcd.group.id[i], j, sep = "."), isfishstmanalysis)
#	    assign(paste("fishprey",tripcd.group.id[i], j, sep = "."), isfishprey)
	    
	    data <- list(istrips = istrips, isfishsets = issets, issetprofile = issetprofile, isgears = isgears, isgearfeatures = isgearfeatures,
	                 iscatches = iscatches, issamples = issamples, isfish = isfish, isfishlengths = isfishlengths, isfishmorphs = isfishmorphs,
	                 isfishstmanalysis = isfishstmanalysis, isfishprey = isfishprey)
	                 
	     
		assign(paste("observer",tripcd.group.id[i], j, sep = "."), data)
		
		save(data, file = paste(data.path,"Observer\\",tripcd.group.id[i],"\\",tripcd.group.id[i],"_",j,".Rdata", sep = ""))
		
		
		
		
		}}
		
ls()
							
						
						
#######################################################################################
###                          GET DATA                                               ###
#######################################################################################

# code tables

isagercodes <- sqlQuery(RODBC_observer,paste("select * from isagercodes;"))
isareadefs <- sqlQuery(RODBC_observer,paste("select * from isareadefs;"))
isbottomcodes <- sqlQuery(RODBC_observer,paste("select * from isbottomcodes;"))
iscountrycodes <- sqlQuery(RODBC_observer,paste("select * from iscountrycodes;"))
isgearcodes <- sqlQuery(RODBC_observer,paste("select * from isgearcodes;"))
isgearfeatureclasses <- sqlQuery(RODBC_observer,paste("select * from isgearfeatureclasses;"))
isgearfeaturecodes <- sqlQuery(RODBC_observer,paste("select * from isgearfeaturecodes;"))
ishaulcommentcodes <- sqlQuery(RODBC_observer,paste("select * from ishaulcommentcodes;"))
ishydrocodes <- sqlQuery(RODBC_observer,paste("select * from ishydrocodes;"))
ismaturitycodes <- sqlQuery(RODBC_observer,paste("select * from ismaturitycodes;"))
ismorphcodes <- sqlQuery(RODBC_observer,paste("select * from ismorphcodes;"))
ismorphvaluecodes <- sqlQuery(RODBC_observer,paste("select * from ismorphvaluecodes;"))
isnafoareas <- sqlQuery(RODBC_observer,paste("select * from isnafoareas;"))
isobservercodes <- sqlQuery(RODBC_observer,paste("select * from isobservercodes;"))
isoperationcodes <- sqlQuery(RODBC_observer,paste("select * from isoperationcodes;"))
issettypecodes <- sqlQuery(RODBC_observer,paste("select * from issettypecodes;"))
issexcodes <- sqlQuery(RODBC_observer,paste("select * from issexcodes;"))
isspeciescodes <- sqlQuery(RODBC_observer,paste("select * from isspeciescodes;"))
isspeciessoughtcodes <- sqlQuery(RODBC_observer,paste("select * from isspeciessoughtcodes;"))
isstrata <- sqlQuery(RODBC_observer,paste("select * from isstrata;"))
istonnageclasscodes <- sqlQuery(RODBC_observer,paste("select * from istonnageclasscodes;"))
istriptypecodes <- sqlQuery(RODBC_observer,paste("select * from istriptypecodes;"))
isownergroups <- sqlQuery(RODBC_observer,paste("select * from isownergroups;"))
isuserprofiles <- sqlQuery(RODBC_observer,paste("select * from isuserprofiles;"))
ispointcodes <- sqlQuery(RODBC_observer,paste("select * from ispointcodes;"))
isproductioncodes <- sqlQuery(RODBC_observer,paste("select * from isproductioncodes;"))
iseezareas <- sqlQuery(RODBC_observer,paste("select * from iseezareas;"))
iscommareas <- sqlQuery(RODBC_observer,paste("select * from iscommareas;"))
isresareas <- sqlQuery(RODBC_observer,paste("select * from isresareas;"))
istechniciancodes <- sqlQuery(RODBC_observer,paste("select * from istechniciancodes;"))
isstartlengthvalues <- sqlQuery(RODBC_observer,paste("select * from isstartlengthvalues;"))
isstmcntntcndcodes <- sqlQuery(RODBC_observer,paste("select * from isstmcntntcndcodes;"))
isstmitemcodes <- sqlQuery(RODBC_observer,paste("select * from isstmitemcodes;"))
isstmitemclasses <- sqlQuery(RODBC_observer,paste("select * from isstmitemclasses;"))





#######################################################################################
###                          Save Data                                              ###
#######################################################################################





codes <- list(	ager = isagercodes, area.defs = isareadefs, bottom = isbottomcodes, country = iscountrycodes,
				gear = isgearcodes, gear.feature.classes = isgearfeatureclasses, gear.feature = isgearfeaturecodes,
				haul.comment =  ishaulcommentcodes, hydro = ishydrocodes, maturity = ismaturitycodes, morph = ismorphcodes,
				morph.value = ismorphvaluecodes, nafo.areas = isnafoareas, observer = isobservercodes, operation = isoperationcodes,
				set.type = issettypecodes, sex = issexcodes, species = isspeciescodes, species.sought = isspeciessoughtcodes,
				strata = isstrata, tonnage.class = istonnageclasscodes, trip.type = istriptypecodes, owner.groups = isownergroups,
				user.profiles = isuserprofiles, point = ispointcodes, production = isproductioncodes, eez.areas = iseezareas,
				comm.areas = iscommareas, res.areas = isresareas, technician = istechniciancodes, start.length.values = isstartlengthvalues,
				stomach.content.condition = isstmcntntcndcodes, stomach.item = isstmitemcodes, stomach.item.classes = isstmitemclasses)
				
				
tm.end <- as.POSIXct(now())	

as.duration(as.interval(tm.start,tm.end))	
				
save(codes, file = "C:\\Users\\comeaupa\\Documents\\My Documents\\r-data\\Observer\\Observer_Codes.Rdata")

#
#		February 11 - fixed RESEARCH tripcd group to include Hagfish 7063, Snow Crab 7064, and Lobster 7065
#		February 11 - added tripcd_id 7099 to groundfish
		February 11 - added tripcd.group lobster which includes the ITQ Survey 7051 and the Lobster Survey 7065
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
