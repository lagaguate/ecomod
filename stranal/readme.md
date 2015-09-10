|	R Result	|	Reference	|	Columns	|	Legacy (APL) STRANAL equivalent worksheet(s)	|
|	------------------------------	|	------------------------------	|	------------------------------	|	------------------------------	|
|	input&#95;parameters	|	results[[1]] 	|	Stranal version; Analysis Date; Experiment Type; Strata; Missions; Year; Species; Wingspread; Distance; Data Source; ALK Modifications; Area;	|	&quot;QUERY&quot;	|
|	strata.areas	|	results[[2]] 	|	TUNITS; SQNM; AreaProp; AreaPropStErr; AreaTot; AreaTotStErr	|	&quot;Strata Area&quot; ; &quot;Prop Area&quot;; &quot;Prop Area Std Err&quot;; &quot;Total Area&quot;; &quot;Total Area Std Area&quot;            	|
|	set&#95;info	|	results[[3]] 	|	MISSION; SEASON; STRAT; SETNO; SDATE; AREA; SLAT; SLONG; DMIN; DMAX; DEPTH; DUR; DIST	|	&lt;New&gt;	|
|	length&#95;by&#95;set	|	results[[4]] 	|	STRAT; MISSION; SETNO; &lt;length bins&gt;; TOTAL	|	&quot;Length by Set&quot;	|
|	length&#95;mean	|	results[[5]] 	|	STRAT; &lt;length bins&gt;; RowTotals	|	&quot;Length Mean&quot;	|
|	length&#95;mean&#95;se	|	results[[6]] 	|	STRAT; &lt;length bins&gt;	|	&quot;Length Mean Standard Error&quot;	|
|	length&#95;total	|	results[[7]] 	|	STRAT; &lt;length bins&gt;	|	&quot;Length Total&quot;	|
|	length&#95;total&#95;se	|	results[[8]] 	|	STRAT; &lt;length bins&gt;	|	&quot;Length Total Standard Error&quot;	|
|	nw&#95;by&#95;set	|	results[[9]] 	|	STRAT; MISSION; SETNO; TOTNO; TOTWGT	|	&quot;Weight By Set&quot;; &lt;New&gt;	|
|	 weights	|	results[[10]]	|	STRAT; COUNT; TOT&#95;WGT; MEAN&#95;WGT; BIOMASS; ST&#95;ERR&#95;WGT; ST&#95;ERR&#95;BIOMASS	|	&quot;Weight Mean&quot;;  &quot;Weight Mean Std Err&quot;;  &quot;Weight Total&quot;; &quot;Weight Total Std Err&quot;	|
|	 numbers	|	results[[11]]	|	STRAT; COUNT; TOT&#95;NO; MEAN&#95;NO; ABUND; ST&#95;ERR&#95;NO; ST&#95;ERR&#95;ABUND	|	&lt;New&gt;	|
|	 age&#95;table	|	results[[12]]	|	&lt;ages&gt;; Length&#95;Totals*&lt;length bins&gt;	|	 &quot;Age Table&quot;	|
|	 age&#95;length&#95;key&#95;totals	|	results[[13]]	|	&lt;ages&gt;*&lt;length bins&gt;	|	&quot;Age Length Key&quot;	|
|	 age&#95;length&#95;weight	|	results[[14]]	|	allFlen; FWT.&lt;ages&gt;*&lt;length bins&gt;	|	&quot;Age Length Weight&quot;	|
|	 age&#95;by&#95;set	|	results[[15]]	|	STRAT; MISSION; SETNO; &lt;ages&gt;	|	&quot;Age By Set&quot;	|
|	 ages	|	results[[16]]	|	STRAT; COUNT; age&#95;&lt;ages&gt;&#95;mean; age&#95;&lt;ages&gt;&#95;se; age&#95;&lt;ages&gt;&#95;tot; age&#95;&lt;ages&gt;&#95;tot&#95;se	|	&quot;Age Mean&quot;; &quot;Age Mean Std Error&quot;; &quot;Age Total&quot;; &quot;Age Total Standard Error&quot;                       	|
