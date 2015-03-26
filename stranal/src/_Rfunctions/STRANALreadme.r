#STRANALgithubreadme.r

#R version of STRANAL
#MMM 2014
#Converted code to a single R file with RODBC (rather than separate SQL)  
#removed requirement for temporary tables ands a schema with write access
#There is still a csv files in the data folder used to load the appropriate 
#trips, though this almost certainly can be done in RODBC
#
#SQL was replaced with merges wherever possible, and filters limit the
#extractions to only the desired missions, strata and species.

#Mark Fowler Aug 2014
#Interactive scripting, jumping around between SQL and R 
#Current example is 4X Haddock in 1974. This was truthed against the 
#original APL version of STRANAL by Stratis Gavaris.
#Intention is to add STRANAL to SurveyScope as a web application. This
#scripting file serves to document the methods, and may suffice if the 
#APL version is lost to us before incorporation into SurveyScope. 
#Apparently IT will not maintain the original.
#Arithmetic precision differs between APL and R. For example we see 
#differences in stratified means beginning about the 3rd decimal
# place. After all the math and rounding and bumping by trawlable 
#units, we see annual totals of 89563863 from The APL STRANAL and
#89563906 from the R STRANAL, a difference of 0.0000005%.
#Using Oracle SQL*Plus (from the DFO SMS) to create custom database 
#objects. Not the only way to do this. Requires an account with access 
#to GROUNDFISH on BANK. 

#Stock identification (species, strata) is hard-coded so must be 
#edited as required for several of the SQL steps.

#Sex and bin width stipulations are the 'official' defaults pulled 
#from groundfish.gsspec. If you want to change these go to the SQL 
#step that creates stock_all_raw_age and follow instructions in 
#comments there. Encountered a conceivable error in the APL STRANAL, 
#but could also be a known and disregarded issue. This is discussed in 
#comments associated with replicating the Length Weight sheet.

#Some scripting is redundant with SurveyScope, but included here so 
#STRANAL can be achieved as a stand-alone job.
