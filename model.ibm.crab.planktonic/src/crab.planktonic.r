
##########################
# read in physical data from Joel's model
#
# ssh choij@142.2.2.160  pw:snow_crab (esquiman,bio.dfo.ca)
#
# grid formats:
#
# 2D grids (m,n)
#   v
#   ^
#   |
#   |---> u  (u is 23 degress counterclockwise from east)
#
#   (v,u) = (0,0) is top-left :
#
# 3D grids (ilo,m,n)
#  eg, u(ilo,m,n), v(ilo,m,n)
#
# in the current run:
#   m = no of v = 226
#   n = no of u = 316
#   ilo = no. of depth layers = 32
#   angle=23 # rotation from cartesian plane
#
#############################

### GSS4 model parameters
  m = 226
  n = 316
  ilo_max= 32
  angle = 23
  mn = m * n
####


	loadfunctions( "model.ibm.crab.planktonic" ) 

  workdir = file.path( ecomod, "model.planktonic" )
  archive = file.path( workdir, "GSS4", "data", "Flux_GSS4_1" )

  setwd( workdir )

  # array structures ..
  U = V = ST = array( NA, dim=c(ilo_max, m, n) )
  # ldep = nlayer = jc = za = array( NA, dim=c(m,n) )
  # dz = rep( NA, ilo_max )

  fn.header = paste( archive, "hdr", sep="." )

  fn = file( fn.header, "rb" )
    header = scan( fn, what="character", sep="\n", n=1)
    times = scan( fn, what="integer", n=6) # sec, min, hour, day, month, year
    names( times) = c( "sec", "min", "hour", "day", "month", "year")
    params = scan( fn, n=4 )   # (dt, m, n ilo)
    dz = scan( fn, n=ilo_max )
    params = c( params,   scan( fn, n=2 ))   # add to list: khor (no. active grids- 2D), ndrei (no active grids 3-D)
    names( params) = c( "dt", "m", "n", "ilo", "khor", "ndrei")
    ldep =  array( scan( fn, n=mn ), dim=c(m,n) )
    nlayer =  array( scan( fn, n=mn ), dim=c(m,n) )
    jc =  array( scan( fn, n=mn ), dim=c(m,n) )
    save.states = scan( fn, n=5 )  # (isave1,isave2,ksave1,ksave2,savelayer )
    names( save.states ) = c("isave1","isave2","ksave1","ksave2","savelayer")
  close( fn)


  water = 1
  one.year = 60 *  60 * 24 * 365 # one year in second
  nt = one.year / params["dt"]  # number of time steps

#  U = extract.3d.field( archive, params, nt=1, save.states, type="u" )
#  V = extract.3d.field( archive, params, nt=1, save.states, type="v" )


  type = "u"

# convert from binary to ascii
octave ::: it works with octave ... something is funny about the "double" class of R

kk = "
fid = fopen('/home/jae/ecomod/model.planktonic/GSS4/data/Flux_GSS4_1.u', 'r'); 
system( 'touch ~/tmp/octave.output' );
while( ! feof (fid ) )
fread( fid, 1, 'int' );
k = fread(fid, 9, 'int' ); 
fread( fid, 1, 'int' ); 
fread( fid, 1, 'int' ) ;
l = fread(fid, k(8), 'real*4' ); 
fread( fid, 1, 'int' ) ;
res = [NaN];
res=[ k; NaN; l; NaN ];
save ('-ascii', '~/tmp/octavedump', 'res' )
system( 'cat ~/tmp/octavedump >> ~/tmp/octave.output ');
endwhile
fclose(fid);

"

system( "rm ~/tmp/octave.*" )
cat( kk, file="~/tmp/octave.tmp" )
system( "octave --quiet --norc ~/tmp/octave.tmp" ) 
system( "mv ~/tmp/octave.output ~/tmp/gss4.u" ) 

# a = scan( "~/tmp/octave.output" )
# system ( "rm ~/tmp/octave.*" )



#################
  type = "v"

# convert from binary to ascii
octave ::: it works with octave ... something is funny about the "double" class of R

kk = "
fid = fopen('/home/jae/ecomod/model.planktonic/GSS4/data/Flux_GSS4_1.v', 'r'); 
system( 'touch ~/tmp/octave.output' );
while( ! feof (fid ) )
fread( fid, 1, 'int' );
k = fread(fid, 9, 'int' ); 
fread( fid, 1, 'int' ); 
fread( fid, 1, 'int' ) ;
l = fread(fid, k(8), 'real*4' ); 
fread( fid, 1, 'int' ) ;
res = [NaN];
res=[ k; NaN; l; NaN ];
save ('-ascii', '~/tmp/octavedump', 'res' )
system( 'cat ~/tmp/octavedump >> ~/tmp/octave.output ');
endwhile
fclose(fid);

"

system( "rm ~/tmp/octave.*" )
cat( kk, file="~/tmp/octave.tmp" )
system( "octave --quiet --norc ~/tmp/octave.tmp" ) 
system( "mv ~/tmp/octave.output ~/tmp/gss4.v" ) 

# a = scan( "~/tmp/octave.output" )
# system ( "rm ~/tmp/octave.*" )




#################
  type = "w"

# convert from binary to ascii
octave ::: it works with octave ... something is funny about the "double" class of R

kk = "
fid = fopen('/home/jae/ecomod/model.planktonic/GSS4/data/Flux_GSS4_1.w', 'r'); 
system( 'touch ~/tmp/octave.output' );
while( ! feof (fid ) )
fread( fid, 1, 'int' );
k = fread(fid, 9, 'int' ); 
fread( fid, 1, 'int' ); 
fread( fid, 1, 'int' ) ;
l = fread(fid, k(8), 'real*4' ); 
fread( fid, 1, 'int' ) ;
res = [NaN];
res=[ k; NaN; l; NaN ];
save ('-ascii', '~/tmp/octavedump', 'res' )
system( 'cat ~/tmp/octavedump >> ~/tmp/octave.output ');
endwhile
fclose(fid);

"


system( "rm ~/tmp/octave.*" )
cat( kk, file="~/tmp/octave.tmp" )
system( "octave --quiet --norc ~/tmp/octave.tmp" ) 
system( "mv ~/tmp/octave.output ~/tmp/gss4.w" ) 

# a = scan( "~/tmp/octave.output" )
# system ( "rm ~/tmp/octave.*" )













  con.u = file( "gss4.u", "r", encoding="UTF-8")
  con.v = file( "gss4.v", "r", encoding="UTF-8")
  con.w = file( "gss4.w", "r", encoding="UTF-8")
  
  
  for ( layer in 1: save.states["savelayer"] ) {
      seek( con.u, where=4 )
      p3 = scan( con.u, what="integer", n=9 ) # "kstep_u", "sec", "min", "hour", "day", "month", "year" 31152
      names(p3) = c("kstep_u",  "sec", "min", "hour", "day", "month", "year", "ncomp", "layer" )
      readBin( fn3, what="raw", n=16 )
      
   #   readBin( fn3, what="raw", n=4 )
      packed = readBin( fn3, what="double", n=p3["ncomp"] )
      readBin( fn3, what="raw", n=4 )

      O = invcompact3( params, layer, K=1, packed )  # K is the scale factor
      Q[layer,,] = O
   
  }



