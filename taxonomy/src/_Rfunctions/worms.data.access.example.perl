# run with perl -f filename

use SOAP::Lite;

my $endpoint = qq{http://www.marinespecies.org/aphia.php?p=soap} ;
my $tns = 'http://aphia/v1.0' ;

my $method_urn = $tns ;
my $soapaction = $tns ;

my $sObj = SOAP::Lite->new(uri => $soapaction, proxy => $endpoint) ;

my $response = $sObj->call(SOAP::Data->name('getAphiaID')->attr({ 'xmlns' => $method_urn})
            => SOAP::Data->name('ScientificName' => 'Solea solea')) ;

print $response->result, "\n";
