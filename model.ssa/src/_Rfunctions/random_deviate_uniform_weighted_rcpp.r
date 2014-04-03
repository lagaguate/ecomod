# sample from the exponential distribution using Rcpp

require(Rcpp)

sourceCpp( rebuild=TRUE, code='
#include <Rcpp.h>
//[[Rcpp::export]]
using namespace Rcpp;

std::vector<unsigned int> random_deviate_uniform_weighted_rcpp ( int nobs, NumericVector probs ) {
  // sample nobs number of indices from a vector of probabilities of length np, without replacement
  // about 500X faster than using "sample"
  int np = probs.size() ;
  std::vector<double> cumprobs( np ) ; // container for the CDF
  std::vector<double> rn =  as< std::vector < double> > ( runif( nobs ) ); 
  std::partial_sum( probs.begin(), probs.end(), cumprobs.begin() );  // compute CDF
  std::vector<unsigned int> J( nobs ) ; 
  int j=0 ;
  int i=0 ;
  std::sort(rn.begin(), rn.end());
  while( i < nobs ) {  // cycle over random numbers and classify reaction in sequence
    while ( j < np &&  i < nobs ) {
        if ( rn[i] < cumprobs[j] ) { 
            J[i] = j+1 ;   // +1 as C uses 0 as first index
            ++i ;
        } else {
            ++j ;
        }
    }
    ++j ;
  }
  return( J );
}')

####

compare = FALSE
if (compare) {
  require(rbenchmark)
  n = 10^3
  np = 10^5
  probs = runif( np  )
  benchmark( 
    random_deviate_uniform_weighted_rcpp( n, probs ),
    .Internal(sample( np, size=n, replace=FALSE, prob=probs ) ), 
    sample( np, size=n, replace=FALSE, prob=probs )  
  )

#                            test replications elapsed relative user.self sys.self user.child sys.child
# 2        -(1/sc) * log(runif(n))          100   7.327    2.640     7.320        0          0         0
# 3                rexp(n, lambda)          100   3.958    1.426     3.955        0          0         0
# 1 sample_exponential_rcpp(n, sc)          100   2.775    1.000     2.771        0          0         0

}
