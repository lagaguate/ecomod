|	R Result	|	Reference	|	Columns	|	Legacy (APL) STRANAL equivalent worksheet(s)	|
|	------------------------------	|	------------------------------	|	------------------------------	|	------------------------------	|
|	input_parameters	|	results[[1]] 	|	Stranal version; Analysis Date; Experiment Type; Strata; Missions; Year; Species; Wingspread; Distance; Data Source; ALK Modifications; Area;	|	"QUERY"	|
|	strata.areas	|	results[[2]] 	|	TUNITS; SQNM; AreaProp; AreaPropStErr; AreaTot; AreaTotStErr	|	"Strata Area" ; "Prop Area"; "Prop Area Std Err"; "Total Area"; "Total Area Std Area"            	|
|	set_info	|	results[[3]] 	|	MISSION; SEASON; STRAT; SETNO; SDATE; AREA; SLAT; SLONG; DMIN; DMAX; DEPTH; DUR; DIST	|	&lt;New&gt;	|
|	length_by_set	|	results[[4]] 	|	STRAT; MISSION; SETNO; &lt;length bins&gt;; TOTAL	|	"Length by Set"	|
|	length_mean	|	results[[5]] 	|	STRAT; &lt;length bins&gt;; RowTotals	|	"Length Mean"	|
|	length_mean_se	|	results[[6]] 	|	STRAT; &lt;length bins&gt;	|	"Length Mean Standard Error"	|
|	length_total	|	results[[7]] 	|	STRAT; &lt;length bins&gt;	|	"Length Total"	|
|	length_total_se	|	results[[8]] 	|	STRAT; &lt;length bins&gt;	|	"Length Total Standard Error"	|
|	nw_by_set	|	results[[9]] 	|	STRAT; MISSION; SETNO; TOTNO; TOTWGT	|	"Weight By Set"; &lt;New&gt;	|
|	 "weights"	|	results[[10]]	|	STRAT; COUNT; TOT_WGT; MEAN_WGT; BIOMASS; ST_ERR_WGT; ST_ERR_BIOMASS	|	"Weight Mean";  "Weight Mean Std Err";  "Weight Total"; "Weight Total Std Err"	|
|	 "numbers"	|	results[[11]]	|	STRAT; COUNT; TOT_NO; MEAN_NO; ABUND; ST_ERR_NO; ST_ERR_ABUND	|	&lt;New&gt;	|
|	 "age_table"	|	results[[12]]	|	&lt;ages&gt;; Length_Totals*&lt;length bins&gt;	|	 "Age Table"	|
|	 "age_length_key_totals"	|	results[[13]]	|	&lt;ages&gt;*&lt;length bins&gt;	|	"Age Length Key"	|
|	 "age_length_weight"	|	results[[14]]	|	allFlen; FWT.&lt;ages&gt;*&lt;length bins&gt;	|	"Age Length Weight"	|
|	 "age_by_set"	|	results[[15]]	|	STRAT; MISSION; SETNO; &lt;ages&gt;	|	"Age By Set"	|
|	 "ages"	|	results[[16]]	|	STRAT; COUNT; age_&lt;ages&gt;_mean; age_&lt;ages&gt;_se; age_&lt;ages&gt;_tot; age_&lt;ages&gt;_tot_se	|	"Age Mean"; "Age Mean Std Error"; "Age Total"; "Age Total Standard Error"                       	|
