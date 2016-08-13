#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rcpp_intToAry(int n, IntegerVector s){
  IntegerVector epsilon(s.size());
  IntegerVector::iterator it;
  it = s.begin(); // ce truc sert à insérer un 1 au début de sizes
  it = s.insert ( it , 1 );
  int G[s.size()]; // ceci je ne peux pas le retourner avec wrap
  // ceci ne marche pas pour la suite : std::vector<int> G (s.size());
  std::partial_sum (s.begin(), s.end(), G, std::multiplies<int>()); // cumprod
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
