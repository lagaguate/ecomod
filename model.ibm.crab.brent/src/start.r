
require("R.oo")
require("geosphere")

rootdir = file.path("C:", "Users", "brent", "Desktop", "RObjects" )
if ( Sys.info()["sysname"] == "Linux"  ) rootdir = file.path("~", "ecomod", "crab.ibm.benthic", "src" )

environ = file.path( rootdir, c("Env.r", "GeoPos.r", "Schedule.r", "SnowCrab.r" ) )

for (e in environ) source(e)

mat = matrix(c(60, 60, 61, 61, 60,  44, 45, 45, 44, 44), ncol = 2, nrow = 5)
bound <- makePoly(mat, interval=1000, r=6378137, sp=FALSE)
env = Environment(mat, 100, 365) # 100 crab for 365 days



