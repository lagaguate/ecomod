
#// template for using net mensuration methods

### -----------------------------------------------------------------------
### -- example usage directly upon a single file, perhaps on board/in situ
### -----------------------------------------------------------------------

# file list can be created and then a gui can be 
# created to use mouse to choose file and run etc ... be my guest .. :)

RLibrary( "lubridate", "sp", "INLA", "Matrix", "splines" ) 
loadfunctions( "utility" )
loadfunctions( "groundfish", "netmensuration")
datadir = file.path( project.datadirectory("groundfish"), "data", "nets", "Scanmar", "datalogs", "2015", "NED2015002" )  # storage location
fn = "NED2015002.028.2015-Mar21-162742.SET.LOG" # filename of data file to examine
fl = file.path( datadir, fn)
mm = load.scanmar.rawdata( fl ) 
bcp = list( id=fn, datasource="groundfish", nr=nrow(mm), tdif.min=9, tdif.max=45, user.interaction=TRUE )  ### yes some are as short as 9 min .. turn user interaction off if you would like automatic solutions only
bcp = bottom.contact.parameters( bcp ) # add other default parameters
bc =  bottom.contact(mm, bcp )
bottom.contact.plot( bc, netspread=TRUE )
str(bc)

