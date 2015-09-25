[arch_login]: http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/eab519dfead1ce94f544182f2ea3500b9dcac999/STRANAL/images/arch_GUI.png "Login screen"
[arch_GUI]: http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/eab519dfead1ce94f544182f2ea3500b9dcac999/STRANAL/images/arch_login.png "GUI"


#Historic Documentation 

**The APL version of STRANAL had documentation associated with it, and that has been captured here.  Please note that the documentation provided on this page relates only to the APL version of STRANAL, and is included here so as to illustrate its evolution.  **

---
[What Is STRANAL?](#What)  
[Downloading and Installation](#Install)  
[How to Use STRANAL?](#How)  
[GUI Components](#GUIComps)  
[ALK Modifications](#alk)  
[Revision History?](#Revisions)  

---


##<a name="What"></a>What Is STRANAL?

STRANAL is a program capable of Stratified Analysis of both DFO and NMFS survey data. STRANAL exports all results to Excel.

STRANAL works by opening a connection to the SABS database. It receives the requested information through standard SQL queries that are generated from user selections through the User Interface. That information is then processed and a Stratified Analysis is created, which is exported to Excel.

STRANAL requires an Oracle account on the SABS database on the QUODDY3 database server located at SABS. New Oracle accounts are requested by filling out a computer account request form. These forms may be obtained from TSD. Select access to the DFO and NMFS data is provided by the respective datasets manager at SABS.

The STRANAL 4.1 application is designed to work with the Run Time System of APL+Win 6.0.10. The Run Time System is freely distributed. STRANAL 4.1 has been tested with Windows XP and Internet Explorer 6.0.28.

~~The STRANAL application and APL+Win may is downloaded and installed from the STRANAL website:~~

~~http://marbiod11/stranal/stranal.html~~

~~Reports of errors and/or recommendations for improvements should be addressed to:~~

~~Lou Van Eeckhaute~~  
~~Fisheries and Oceans Canada~~  
~~531 Brandy Cove Road~~  
~~St. Andrews, NB E5B 2L9~~  
~~CANADA~~  
~~e-mail: Van-EeckhauteL@mar.dfo-mpo.gc.ca~~  
~~Tel.: (506)529-5938~~  
~~Fax: (506)529-5862~~  

##<a name="Install"></a>Downloading and Installation
The download files are self-extracting when executed. If you place the files in directories other than those suggested, you must modify the short-cut command line accordingly. After downloading and extracting the files, double-click the short-cut to execute STRANAL.

STRANAL is written in the APL programming language. The APL Run Time System is distributed free of charge here and permits you to use all the features of STRANAL but you will not be able to modify the program. A licensed copy of the APL Development System is required to use the STRANAL source code.

~~To receive notification of software updates, please subscribe as a user with Lou Van Eeckhaute.~~ The STRANAL software is provided free of charge and the providers assume no responsibility for its use or results obtained with it.

Downloads

|Release Date | STRANAL Run Time | APL Run Time | STRANAL Source Code |
|---|---|---|---|
|JAN 2012 | <a href="http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/c844101a518ba68a4f0914460adcb4e0178536bb/stranal/archived/stranal4_1_install.exe">4.1</a> | <a href="http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/c844101a518ba68a4f0914460adcb4e0178536bb/stranal/archived/aplwr6_0_install.exe">6.0</a> |<a href="http://gitlab.ssc.etg.gc.ca/mcmahon/PED_Analytics/blob/c844101a518ba68a4f0914460adcb4e0178536bb/stranal/archived/stranal4_1_src_install.exe">4.1</a>|

##<a name="How"></a>How to Use STRANAL?
From Windows desktop select Start > Programs > STRANAL > STRANAL 4.1. The database logon form will be displayed. 
![Database login screen][arch_login]

User Name and Password is your Oracle account name and password for the database supplied in the Database field. The Database refers to the database instance where access to related DFO and NMFS bottom trawl data is available. Enter the information and click on the OK button or click the Cancel button to stop the application. The logon information and data access is checked and if accepted the main selection form is displayed with the DFO data initial selected by default.

![GUI][arch_GUI]

Refer to the GUI Components section for details on specifying analysis criteria.

Multiple item selections can be made from the Survey, Unit Area, Strata, and Exp Type lists. To select a range of items first click on the first item in a desired range then Shift+Click on the last item of the range. Items can added to your selection by using Ctrl+Click. 

Once the selection criteria is set, Click on the Execute button. The program will query the appropriate data and send the results to Excel. The program will alert you with popup messages if any errors occur. 

**NOTE:**For DFO Herring data, species code 60, the fish length extracted from the database is Fork Length. This is converted to total length during processing. TotalLength=(1.0866×FishLength)+0.95632.

##<a name="GUIComps"></a>GUI Components
###DFO or NMFS
Choose whether you wish to perform an analysis using DFO data or NMFS data. The choice will fill in the selection lists for Mission(Survey tab), Unit Area, Strata, and Species. This process may take several moments for the NMFS data. 
###Title
Enter a title for your analysis. It will be displayed on the Query Sheet, the first sheet in the resulting Excel Workbook. 
###Survey
The stratified analysis is conducted on data collected during a survey. A survey may consist of one or more missions. A list of available missions for each of the databases allows you choose the mission(s) that you want included in the analysis. Multiple mission selections are used in the queries as: *.Mission in (‘mission1’,’mission2’). 
When the missions are selected for a specific survey you will notice a new Survey tab will be created. This permits analysis for multiple surveys during the same execution using common criteria from the other selection components. Each survey analysis will result is a separate Excel Workbook.
###Unit Area
A list from which you select a subset of the selected Unit Area. Multiple selections are allowed, and are used in the queries as: *.Area in (‘551’,’552’) 
###Strata
A list from which you select the strata to be queried. Multiple selections are allowed, and are used in the queries as: *.Strata in ('5Z1','5Z2') 
###Species
The species to be analyzed. Only a single species may be selected. 
###By Sex
If the fish was measured by sex, then this box will enable you to perform calculations by sex when enabled. 
###Stratum Area Table
Only sets that lie within the strata selected AND the unit areas selected will be included in the analysis. In order the stratum means are weighted and extrapolated properly for totals. The appropriate areas for selected strata or selected portions of strata are obtained from a ‘stratum area table’. Several pre-defined ‘stratum area table’ choices, described below, are available from the drop down list. If none of these are suitable for the desired analysis, a user defined ‘stratum area table’ may be specified by simply entering the table name for a table in your Oracle user account. The required fields for the table are described below.

**GROUNDFISH.GSSTRATUM** - This is the default value. The strata areas reflect the total strata areas for strata used in the DFO bottom trawl surveys and for strata used in the NMFS bottom trawl surveys. This table should be used when there is no unit area selection.

**USNEFSC.DFO5ZJM** – The strata areas reflect the portion of the DFO strata areas within unit areas 523 (5Zej) and 524 (5Zem). This table can be used when unit areas 523 and 524 have been selected and the analysis is to be done on 5Zjm, 5Zc or 5Zu. To analyze unit areas 5Zjm the appropriate strata to select are 5Z1 to 5Z4. To analyze NAFO Sub-division 5Zc, the Canadian portion of 5Ze/5Zjm, the appropriate strata to select are 5Z1 and 5Z2. To analyze the USA portion of unit areas 5Zjm, the appropriate strata to select are 5Z3 and 5Z4. 

**USNEFSC.DFO5ZGHNO** - The strata areas reflect the portion of the DFO strata areas within unit areas 521 (5Zeg), 522 (5Zeh), 525 (5Zen) and 526 (5Zeo). This table can be used when unit areas 521, 522, 525 and 526 have been selected and the analysis is to be done on 5Zghno. To analyze unit areas 5Zghno the appropriate strata to select are 5Z3 - 5Z8. 

**USNEFSC.NMFS5ZJM** - The strata areas reflect the portion of the NMFS strata areas within unit areas 561 and 551 (5Zej) and 562 and 552 (5Zem).  This table can be used when unit areas 551, 552, 561 and 562 have been selected and the analysis is to be done on 5Zjm. To analyze unit areas 5Zjm the appropriate strata to select are 01160, 01170, 01180, 01190, 01200, 01210 and 01220. 

**USNEFSC.NMFS5ZGHNO** - The strata areas reflect the portion of the NMFS strata areas within unit areas 521 (5Zeg), 522 (5Zeh), 525 (5Zen) and 526 (5Zeo).  This table can be used when unit areas 521, 522, 525 and 526 have been selected and the analysis is to be done on 5Zghno. To anlayze unit areas 5Zghno the appropriate strata to select are 01130, 01140, 01150, 01190, 01200, 01210, 01220, 01230, 01240 and 01250. 

**UNNEFSC.NMFS5ZJMC** - The strata areas reflect the portion of the NMFS strata areas within NAFO Sub-division 5Zc, the Canadian portion of 5Ze/5Zjm. This table can be used when unit areas 551 and 552 have been selected and the analysis is to be done on 5Zc. To anlayze unit areas 551 and 552 the appropriate strata to select are 01160, 01170, 01180, 01210 and 01220. 

**USNEFSC.NMFS5ZJMU** - The strata areas reflect the portion of the NMFS strata areas within unit areas 561 and 562, the USA portion of 5Zjm. To analyze the USA portion of unit areas 5Zjm, the appropriate strata to select are 01160, 01170, 01180, 01190, 01200, 01210 and 01220. 

**USNEFSC.NMFS5ZU** - The strata areas reflect the portion of the NMFS strata areas within unit areas 521 (5Zeg), 522 (5Zeh), 525 (5Zen), 526 (5Zeo), 561 (5Zeju) and 562 (5Zemu), the USA portion of 5Z. This table can be used when unit areas 521, 522, 525, 526, 561 and 562  have been selected and the analysis is to be done on the USA portion of 5Z. To analyze the USA portion of 5Z the appropriate strata to select are 01130, 01140, 01150, 01160, 01170, 01180, 01190, 01200, 01210, 01220, 01230, 01240 and 01250.

The pre-defined choices are summarized in the table below. 

|Survey Series	|Description		|Stratum Area Table		|Unit Areas						|Strata
|---|---|---|---|---|
|DFO or NMFS	|Complete strata	|GROUNDFISH.GSSTRATUM	|none selected					|Any combination of DFO or NMFS strata
|DFO			|5Zjm				|USNEFSC.DFO5ZJM		|523, 524						|5Z1-4
|DFO			|5Zc				|USNEFSC.DFO5ZJM		|523, 524						|5Z1 & 5Z2
|DFO			|5Zjmu				|USNEFSC.DFO5ZJM		|523, 524						|5Z3 & 5Z4
|DFO			|5Zghno				|USNEFSC.DFO5ZGHNO		|521, 522, 525, 526				|5Z3-5Z8
|NMFS			|5Zjm				|USNEFSC.NMFS5ZJM		|551, 552, 561, 562				|01160-01220
|NMFS			|5Zghno				|USNEFSC.NMFS5ZGHNO		|521, 522, 525, 526				|01130/01150,01190/01250
|NMFS			|5Zc				|UNNEFSC.NMFS5ZJMC		|551,552						|01160/10180, 01210, 01220
|NMFS			|5Zjmu				|USNEFSC.NMFS5ZJMU		|561, 562						|01160/01220
|NMFS			|5Zu				|USNEFSC.NMFS5ZU		|521, 522, 525, 526, 561, 562	|01130/01250

User defined stratum area tables with appropriate area values for other combinations can be specified, e.g. to analyze the Canadian portion of 5Zj the area, in sq. nmiles, of 5Z1 and 5Z2 that lies within 5Zj must be calculated to use in the 'AREA' column of the user defined table. The  STRANAL run should then be done with the user defined table name, unit area 523 selected and strata 5Z1 and 5Z2 selected. The user defined table must contain the following fields: STRAT (stratum) and AREA. The value in the 'Area' column holds the size of a stratum in square nautical miles and is used to calculate the trawlable units. 
###Distance
Enter the required distance towed.
###Spread
The wingspread in feet. Initialized to 41 for DFO data, and initialized to 34 for NMFS data. A new value may be entered if needed. 
###Experiment Types
For DFO data: A list from which you select Experiment types. Multiple selections are allowed, and are used in the queries as: *.xtype in (‘1’,’2’) 
For NMFS data: A text field containing the experiment type value. Any experiment type greater than or equal to the value will be included in the analysis. The default is <= 136. This may be changed as required. Used in the queries as: *.SHG <= 136
###Vessel Net Conversion
If checked a conversion factor is applied to NMFS count at length data for any matching Vessel, Species and Length combination found in the lookup table named USNEFSC.US_VESSEL_NET_CONVERSIONS. If the Vessel and Species match and the Length do not then the conversion factor of the maximum length is used. 


##<a name="alk"></a>ALK Modifications
###Use All Experiment Types for ALK
Enable if you wish to use data from all experiment types. This option applies only to the Age Length Key modification query. 
###ALK Unit Area
Choose Unit Area for the Age Length Key modification query.
###ALK Strata
Choose Strata for the Age Length Key modification query. 
###ALK Modification Table
The table name for the table that fills in Age Length Key lengths without age data. Must include fields SURVEY, FLEN, FSEX, AGE and CLEN. If the table resides on the user’s Oracle account, simply entering the table name is sufficient. If the table resides on another account then both the account name and table name are required. Example: Johndoe.personaltable 


##<a name="Revisions"></a>Revision History
|Version|Notes	|
|---	|---		|
|	1	|	<ul><li>initial release</li></ul>	|
|	1.1	|	<ul><li>fixed handling of multiple missions for DFO data, only single mission selection permitted for NMFS data</li><li>fixed conversion of column number to Excel RC letter designation</li></ul>	|
|	1.2	|	<ul><li>modified OPEN_NMFS_DAO, BUILD_NMFS_QUERY and POPULATE functions to work with revised data views that access pre and post FSCS US survey data (new datatypes and column names)</li><li>recreated table USSPEC with new SPEC datatype (number to varchar2 to match SVSPP) and modified BUILD_NMFS_QUERY for change of SVSPP from number to varchar2 datatype</li><li>changed pointer to hourglass while querying Oracle tables to fill in mission, area and strata boxes on form (in BUILD_NMFS_QUERY and BUILD_DFO_QUERY)</li></ul> 	|
|	1.3	|	<ul><li>modified OPEN_NMFS_DAO, BUILD_NMFS_QUERY and POPULATE functions to work with Oracle views that access new snapshots created by NMFS which combine FSCS and pre-FSCS data</li><li>uses new field, CRUISE6, to specify cruise codes</li></ul> 	|
|	1.4	|	<ul><li>created local views of remote US datatables without the ROWID (RD) field and replaced the views that STRANAL uses to point to these newly created local views with the ROWID (RD) field added; this was needed to accommodate an incompatibility between Oracle 8.1 and SQLNET 8.0 when using MSAccess to link to remote database tables</li><li>recreated SURVEY_ODBC.mdb to link to the replaced US data views and the new dsn sabs.quoddy2</li><li>corrected error when user specified alk modification table was used; modified GET_DATA</li><li>corected error in how length grouping was handled for sexed species when length grouping was not 1 (e.g. winter skate); modified MAKE_LF</li><li>added 2 variables "dsn" and "dbq" to hold the name of the ODBC service; BUILD_DFO_QUERY and BUILD_NMFS_QUERY were modified to use the variables holding the name for the ODBC service</li></ul>	|
|	1.5	|	<ul><li>EXEC was modified to erase alkmod which was being left from the previous run when multiple surveys were executed</li><li>when a survey was not listed in the alk modification table, calculations were attempted with a null variable "alkmodtable" which caused an error so a test was added in STRATIFIED to check for an empty variable which replaced the test for text in the alk modification box</li><li>removed text "SABS2" from the popup error message when database is not accessible</li><li>area occupied calculation added:</li><li>Travis Shepherd wrote 2 new functions (AREA and AREA_STRATUM) which calculate area occupied and modified EXEC and CREATE_OUTPUT</li><li>LVE wrote EXTRACT_COL to extract a column from a nested array without padding with 0’s</li></ul>	|
|	1.6	|	<ul><li>changes needed to work with Windows XP</li><li>OPEN_DFO_DAO and NMFS_DFO_DAO modified to refer to DAO.DBEngine.36 instead of DAO.DBEngine.35</li><li>XL_INIT à changed ‘value’ to ‘value2’ (property of Range)</li></ul>	|
|	1.7	|	<ul><li>changes made to speed up populating of STRANAL form with US survey pick lists</li><li>form is now populated from local tables which contain only the "distinct" values needed for the survey, stratum and area pick lists (in OPEN_NMFS_DAO)</li><li>added ORDER_BY clause to query to select mission and species for pick lists when NMFS surveys specified</li></ul>	|
|	2	|	**Dec-06**<ul><li>Access to Oracle database changed to Microsoft's newest high-level interface for data objects ADO (ActiveX Data Objects). Previous access used DAO (Data Access Objects).</li><li>Excel spreadsheet displays properly when a workbook is closed followed immediately by another analysis output.</li><li>Blank cells replace zeros for age length key output to Excel.</li><li>Version number, date and time of analysis added to output.</li><li>Logon form modified and default behaviour of buttons changed when Enter key is used.</li><li>Extra detail included for ALK modification for non selected experiment types when the all experiment type check box is selected.</li><li>Database queries for detail modified to remove the not null age check from both DFO and NMFS data. Added check for not null fish number or not null weights for DFO data only.</li><li>Added an edit box for NMFS experiment type. Defaults to <=136 default.</li><li>Duplicate detail data is removed where the ALK modification selection overlaps with the main selection.</li><li>Vessel code concatenated to cruise6 and set number for NMFS set identifier.</li><li>Unit Area added to processing and by set output.</li><li>Multiple size classes in DFO data now handled.</li><li>Error handling modified to ease error checking when using the development workspace.</li><li>Application deployed and installed via a web download site.</li><li>Converts Herring (species code 60) Fork Length to Total length for DFO data only. Total Length = (1.0866*FishLength)+0.95632</li><li>Added output sheet with stratum area.</li><li>Added stratum area table options to drop-down list.</li><li>Added a list for selection of DFO experiment type.</li><li>Removed check box to request average length and average weight calculations. Done automatically if data is available.</li><li>Modified average weight at length calculations to include all available records (previously used only records with a valid age).</li><li>Average length and average weight at age calculations done by sex as well as combined.</li></ul>	|
|	2.1	|	**Mar-07**<ul><li>Access to Oracle database changed to use the most recent version of Microsoft's ActiveX Data Objects (ADO) available to the desktop’s COE (Common Operating Environment) rather than reference a specific version.</li></ul>	|
|	3	|	**May-07**<ul><li>Updated to use APL+Win 6.0. Include processing of sets with zero sample and total weights.</li></ul>	|
|	4	|	**Mar-11**<ul><li>Option added to apply US Vessel Net Conversion factors to count at length for matching Vessel, Species and Length located in a conversion table.</li></ul>	|
|	4.1	|	**Jan-12**<ul><li>Connectivity changed from MDAC (Microsoft Data Access Components) to ODAC (Oracle Data Access Components)</li></ul>|



