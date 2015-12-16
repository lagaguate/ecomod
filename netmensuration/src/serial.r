

loadfunctions( "netmensuration") 

con = serialbus.connect( list( port="COM1", baudrate=9600, parity="E", databits=8, stopbit=1 ) )

# system( "mode COM1:4800,E,7,1" ) # test for windows

con = file( "COM1", open="r" )
outfile = "test.txt"

if (isOpen(con)) {
  cat( paste( "Starting logging of serial bus::", date()), file=outfile ) 
  while (1) {
    res = scan( con, n=1, quiet=TRUE, what="character")
    cat( res, file=outfile, append=TRUE ) 
  }
}


