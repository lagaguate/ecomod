################################################################################
# Script: ClamPDFversion7.r, output is a PDF file "testdf7.pdf" saved on desktop
################################################################################
# This is R code for connecting to the Offshore Clam Oracle database and
# producing commercial indices of the Arctic Surfclam stock status in relation
# to "trigger levels" that would indicate a closer look at the stock status
# is warranted.
# The script is presently set up to do Banquereau and Grand Bank, 
# in a main fuction LoopThroughPlots(Sel.Bank, fig.num, log.data) with 
# variable Sel.Bank = 1 or 2 to select which bank,
# Sel.Bank <- 1 is for Banquereau, Sel.Bank <- 2 is for Grand Bank
################################################################################
# Required libraries (called as needed)
# ------------------------------------------------------------------------------
# tcltk      - used for password function and interactive input of final year
# RODBC      - used for database connection
# PBSmapping - used for map drawing
# reshape    - used for cumulative sums
# Plotrix    - used for multiple plots per page
# grid       -
# gridExtra  - 

##############################################################################
  # Make connection to Offshore Clam Oracle database on Bank
  ##############################################################################

  #   Note: Have to have previously setup Clam_DB in System DSN
  #   You only have to set this up once:
  #   Go to Start\Controp Panel\System and Security\Administrative tools\
  #       Data Sources (ODBC) 
  #   Select "System DSN" tab, click "Add", in the window that pops up  
  #   scroll down to "Oracle in OraClient10g2",
  #   click Finish, "Oracle ODBC Driver Configuration" window pops up,
  #   in "TSN Service Name" tab scroll down to BANK; put in your User ID 
  #   for Oracle, put in a Data Source Name that you will 
  #   call from R and add a description if you want.  
  #   When you test the connection it will ask for your password.
  #   Once this is set up and you can make the connection click OK
  #   You are now ready to connect from R
  ##############################################################################
# To run the file in ecomod, run the following commands

loadfunctions("offshoreclams")
loadfunctions("polygons")

RLibrary( "gridExtra", "PBSmapping", "reshape", "plotrix" ) # Load required packages

year.of.interest = 2015 # define or some other final year
output.directory = file.path( project.datadirectory("offshoreclams"), "docs" )


ClamPDF( year.of.interest, outdir=output.directory )  


