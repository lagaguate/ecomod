|	R Result	|	Reference	|	Columns	|	Legacy (APL) STRANAL equivalent worksheet(s)	|
|	------------------------------	|	------------------------------	|	------------------------------	|	------------------------------	|
|	input&#95;parameters	|	results[[1]] 	|	Stranal version; Analysis Date; Experiment Type; Strata; Missions; Year; Species; Wingspread; Distance; Data Source; ALK Modifications; Area;	|	"QUERY"	|
|	strata.areas	|	results[[2]] 	|	TUNITS; SQNM; AreaProp; AreaPropStErr; AreaTot; AreaTotStErr	|	"Strata Area" ; "Prop Area"; "Prop Area Std Err"; "Total Area"; "Total Area Std Area"            	|
|	set&#95;info	|	results[[3]] 	|	MISSION; SEASON; STRAT; SETNO; SDATE; AREA; SLAT; SLONG; DMIN; DMAX; DEPTH; DUR; DIST	|	&lt;New&gt;	|
|	length&#95;by&#95;set	|	results[[4]] 	|	STRAT; MISSION; SETNO; &lt;length bins&gt;; TOTAL	|	"Length by Set"	|
|	length&#95;mean	|	results[[5]] 	|	STRAT; &lt;length bins&gt;; RowTotals	|	"Length Mean"	|
|	length&#95;mean&#95;se	|	results[[6]] 	|	STRAT; &lt;length bins&gt;	|	"Length Mean Standard Error"	|
|	length&#95;total	|	results[[7]] 	|	STRAT; &lt;length bins&gt;	|	"Length Total"	|
|	length&#95;total&#95;se	|	results[[8]] 	|	STRAT; &lt;length bins&gt;	|	"Length Total Standard Error"	|
|	nw&#95;by&#95;set	|	results[[9]] 	|	STRAT; MISSION; SETNO; TOTNO; TOTWGT	|	"Weight By Set"; &lt;New&gt;	|
|	 "weights"	|	results[[10]]	|	STRAT; COUNT; TOT&#95;WGT; MEAN&#95;WGT; BIOMASS; ST&#95;ERR&#95;WGT; ST&#95;ERR&#95;BIOMASS	|	"Weight Mean";  "Weight Mean Std Err";  "Weight Total"; "Weight Total Std Err"	|
|	 "numbers"	|	results[[11]]	|	STRAT; COUNT; TOT&#95;NO; MEAN&#95;NO; ABUND; ST&#95;ERR&#95;NO; ST&#95;ERR&#95;ABUND	|	&lt;New&gt;	|
|	 "age&#95;table"	|	results[[12]]	|	&lt;ages&gt;; Length&#95;Totals*&lt;length bins&gt;	|	 "Age Table"	|
|	 "age&#95;length&#95;key&#95;totals"	|	results[[13]]	|	&lt;ages&gt;*&lt;length bins&gt;	|	"Age Length Key"	|
|	 "age&#95;length&#95;weight"	|	results[[14]]	|	allFlen; FWT.&lt;ages&gt;*&lt;length bins&gt;	|	"Age Length Weight"	|
|	 "age&#95;by&#95;set"	|	results[[15]]	|	STRAT; MISSION; SETNO; &lt;ages&gt;	|	"Age By Set"	|
|	 "ages"	|	results[[16]]	|	STRAT; COUNT; age&#95;&lt;ages&gt;&#95;mean; age&#95;&lt;ages&gt;&#95;se; age&#95;&lt;ages&gt;&#95;tot; age&#95;&lt;ages&gt;&#95;tot&#95;se	|	"Age Mean"; "Age Mean Std Error"; "Age Total"; "Age Total Standard Error"                       	|
