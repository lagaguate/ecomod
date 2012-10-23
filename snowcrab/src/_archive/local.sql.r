
# ----------------------------------------------------------------------------
  # define the database engine connection
  # valid are: server = "gs_rama" 
  #            server = "gs_shevek"
  #            server = "gs_local"  .. the local machine
  # look in ~/.my.cnf for correct values
  # ----------------------------------------------------------------------------

  drv = dbDriver("MySQL")

  gs = dbConnect(drv, dbname="gs")
  ec = dbConnect(drv, dbname="ecnasap")
  sc = dbConnect(drv, dbname="snowcrab")
 
  join.tables = function (table) {

    if table == "scs" {
      dbSendQuery(sc, "drop table if exists scs_all")

      dbSendQuery(sc, paste(
          'create table scs_all',
          'select m.* , trawl_area, distance,', 
          '  start_latitude, end_latitude, start_longitude, end_longitude,',
          '  total_males, total_females, total_crabs  ',
          'from scs_main as m ',
          'left join scs_area as a on, ',
          '  ( m.tow_date = a.tow_date and m.trawl_number=a.trawl_number )'
      ) )
      
      out = dbGetQuery(sc, "select * from scs_all")
    }

     if table == "" {
      dbSendQuery(sc, "drop table if exists scs_all")

      dbSendQuery(sc, paste(
          'create table scs_all',
          'select m.* , trawl_area, distance,', 
          '  start_latitude, end_latitude, start_longitude, end_longitude,',
          '  total_males, total_females, total_crabs  ',
          'from scs_main as m ',
          'left join scs_area as a on, ',
          '  ( m.tow_date = a.tow_date and m.trawl_number=a.trawl_number )'
      ) )
   
      out = dbGetQuery(sc, "select * from scs_all")
    }

   
    
     }

  scs = join.scs()





