#include <HsFFI.h>


/**
 * Optimized version of vector by matrix multiplication with 32 bit floating point numbers
 * It also allows to focus on multiplying only by a subregion of a matrix
 */
void mult_vec_mat(const HsFloat *vec,
                  const HsFloat *mat,
                  HsInt m,
                  HsInt n,
                  HsFloat* vec_res,
                  HsInt from_col_mat,
                  HsInt to_col_mat) {
  HsFloat x;
  HsInt skipped = n - (to_col_mat - from_col_mat);
  if(m > 0){
    x = vec[0];
    mat+= from_col_mat;
    for(HsInt j = from_col_mat; j < to_col_mat; j++){
      vec_res[j] = x * *(mat++);
    }
    mat+= skipped;
    for(HsInt i = 1; i < m; i++){
      x = vec[i];
      for(HsInt j = from_col_mat; j < to_col_mat; j++){
        vec_res[j]+= x * *(mat++);
      }
      mat+= skipped;
    }
  }
}


// When compiled with -O3 `mult_vec_mat` is vectorized automatically by the compiler,
// which results in the same performance and machine code that closely resembles the hand
// optimized code below:

/*
void mult_dense_x2(const float *vec, const float *mat, int m, int n, float* vec_res) {
  int64_t width = 8;
  __m256 x, y, z, v;
  for(int64_t j = 0; j < n; j++)
    vec_res[j] = 0;
  for(int64_t i = 0; i < m; i++){
    x = _mm256_broadcast_ss(&vec[i]);
    for(int64_t j = 0; j < n; j+= width){
      y = _mm256_loadu_ps(mat);
      z = _mm256_loadu_ps(vec_res + j);
      v = _mm256_add_ps(z, _mm256_mul_ps(x, y));
      _mm256_storeu_ps(vec_res + j, v);
      mat+= width;
    }
  }
}
*/
