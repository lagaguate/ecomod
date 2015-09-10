|	R Result	|	Reference	|	Columns	|	Legacy (APL) STRANAL equivalent worksheet(s)	|
|	------------------------------	|	------------------------------	|	------------------------------	|	------------------------------	|
|	input_parameters	|	results[[1]] 	|	Stranal version; Analysis Date; Experiment Type; Strata; Missions; Year; Species; Wingspread; Distance; Data Source; ALK Modifications; Area;	|	"QUERY"	|
|	strata.areas	|	results[[2]] 	|	TUNITS; SQNM; AreaProp; AreaPropStErr; AreaTot; AreaTotStErr	|	"Strata Area" ; "Prop Area"; "Prop Area Std Err"; "Total Area"; "Total Area Std Area"            	|
|	set_info	|	results[[3]] 	|	MISSION; SEASON; STRAT; SETNO; SDATE; AREA; SLAT; SLONG; DMIN; DMAX; DEPTH; DUR; DIST	|	<New>	|
|	length_by_set	|	results[[4]] 	|	STRAT; MISSION; SETNO; <length bins>; TOTAL	|	"Length by Set"	|
|	length_mean	|	results[[5]] 	|	STRAT; <length bins>; RowTotals	|	"Length Mean"	|
|	length_mean_se	|	results[[6]] 	|	STRAT; <length bins>	|	"Length Mean Standard Error"	|
|	length_total	|	results[[7]] 	|	STRAT; <length bins>	|	"Length Total"	|
|	length_total_se	|	results[[8]] 	|	STRAT; <length bins>	|	"Length Total Standard Error"	|
|	nw_by_set	|	results[[9]] 	|	STRAT; MISSION; SETNO; TOTNO; TOTWGT	|	"Weight By Set"; <New>	|
|	 "weights"	|	results[[10]]	|	STRAT; COUNT; TOT_WGT; MEAN_WGT; BIOMASS; ST_ERR_WGT; ST_ERR_BIOMASS	|	"Weight Mean";  "Weight Mean Std Err";  "Weight Total"; "Weight Total Std Err"	|
|	 "numbers"	|	results[[11]]	|	STRAT; COUNT; TOT_NO; MEAN_NO; ABUND; ST_ERR_NO; ST_ERR_ABUND	|	<New>	|
|	 "age_table"	|	results[[12]]	|	<ages>; Length_Totals*<length bins>	|	 "Age Table"	|
|	 "age_length_key_totals"	|	results[[13]]	|	<ages>*<length bins>	|	"Age Length Key"	|
|	 "age_length_weight"	|	results[[14]]	|	allFlen; FWT.<ages>*<length bins>	|	"Age Length Weight"	|
|	 "age_by_set"	|	results[[15]]	|	STRAT; MISSION; SETNO; <ages>	|	"Age By Set"	|
|	 "ages"	|	results[[16]]	|	STRAT; COUNT; age_<ages>_mean; age_<ages>_se; age_<ages>_tot; age_<ages>_tot_se	|	"Age Mean"; "Age Mean Std Error"; "Age Total"; "Age Total Standard Error"                       	|
