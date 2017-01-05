#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
IntegerVector rcpp_intToAry(int n, IntegerVector s){
  // will contain the output:
  IntegerVector epsilon(s.size());
  // insert 1 at the beginning of s:
  IntegerVector::iterator it;
  it = s.begin();
  it = s.insert ( it , 1 );
  // vector of cumulative products:
  int G[s.size()];
  std::partial_sum (s.begin(), s.end(), G, std::multiplies<int>());
  // greedy algorithm:
  int k;
  while(n>0){
    k=1;
    while(G[k]<=n){
      k=k+1;
    }
    epsilon[k-1] = (int)n / G[k-1];
    n = n % G[k-1];
  }
  return(epsilon);
}
