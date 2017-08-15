#include <math.h>
#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector CountUniqueC(NumericVector value, NumericVector group){
  double n = group.size();
  int x, id_start, id_end, old_key_value;
  NumericVector n_vec = int(n);
  if(group.size() == 1){
    NumericVector v = unique(value);
    x = v.size() - any(is_na(v));
    n_vec(0) = x;
    return n_vec;
  }
  // // std::cout << 'WARNING: the key assumption of CountUniqueC is' << 'that group is presorted\n';
  old_key_value = group(0);
  id_start = 0;
  for(int i = 1; i < n; i++){
    if(group(i) != old_key_value){
      id_end = i;
      NumericVector v = unique(value[seq(id_start, id_end - 1)]);
      x = v.size() - any(is_na(v));
      for(int j = id_start; j < id_end; j ++) n_vec(j) = x;
      id_start = i;
      old_key_value = group(i);
    }
  }
  id_end = n;
  NumericVector v = unique(value[seq(id_start, id_end - 1)]);
  x = v.size() - any(is_na(v));
  for(int j = id_start; j < id_end; j ++) n_vec(j) = x;
  return n_vec;
}


//' @export
// [[Rcpp::export]]
NumericVector GenerateGroup(NumericVector id, NumericVector start, NumericVector end){
  int n = id.size();
  NumericVector newrecord(n);
  NumericVector round_end(n), round_start(n);
  round_end = seq_len(n) - 1;
  round_start = seq_len(n) - 1;
  newrecord = seq_len(n) - 1;
  // double cur_id = id(0);
  for(int i = n-2; i >= 0; i--){
    if(id(i) == id(i+1)){
      round_end(i) = round_end(i+1);
    }
  }
  for(int i = 0; i < n; i++){
    for(int j = i; j <= round_end(i); j++){
      if(id(i) == id(j)){
        bool in_between_1 = (start(i) < end(j)) && (end(i) >= end(j));
        bool in_between_2 = (start(i) <= start(j)) && (end(i) > start(j));
        bool in_between_3 = (start(i) == end(i)) && (end(j) >= start(i)) && (start(j) <= start(i));
        if(in_between_1 || in_between_2 || in_between_3){
          newrecord(j) = std::min(newrecord(i), newrecord(j));
          newrecord(i) = newrecord(j);
        }
      } else {
        continue;
      }
    }
  }
  return newrecord;
}
