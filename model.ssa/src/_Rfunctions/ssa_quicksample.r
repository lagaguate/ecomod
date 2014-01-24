  
# find first reaction channel from an SSA .. two versions ... ssa_sample_direct is the fastest



# FASTEST -- directly use C++
require(Rcpp)
sourceCpp( rebuild=TRUE, code='
#include <Rcpp.h>

inline int randWrapper(const int n) { return floor(unif_rand()*n); }
using namespace std;
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector ssa_sample_direct( NumericVector probs, NumericVector rn ) {
  int np = probs.size() ;
  int num = rn.size() ;
  IntegerVector J(num) ;
	int j=0 ;
  int i=0 ;
  while( i < num ) {  // cycle over random numbers and classify reaction in sequence
    while ( j < np &&  i < num ) {
      if ( rn[i] < probs[j] ) { 
        // debug
        // Rcpp::Rcout << i << " " << j << " " << rn[i] << " " << probs[j] << " \\n " ;
        J[i] = j+1 ;   // +1 as C uses 0 as first index
        i++ ;
      } else {
        j++ ;
      }
    }
    j++ ;
  }
  random_shuffle(J.begin(), J.end(), randWrapper);  //randomize order
  return( wrap( J ) );  
}
')



debug = FALSE
if ( debug) {
  v = cumsum(runif(20))
  v = v/max(v)
  ru = runif(10) 
  rno = order( ru)
  ssa_sample_direct( v, sort(ru ) )
  for (i in 1:10000) ssa_sample_direct( v, sort(ru ) )
}




