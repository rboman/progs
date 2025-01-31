#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

void saxpy(int n, float a, float *x, float *y) {
  double elapsed = -1.0 * omp_get_wtime();

  // We don't need to map the variable a as scalars are firstprivate by default
  #pragma omp target teams distribute parallel for \
                     map(to:x[0:n]) map(tofrom:y[0:n])
  for(int i = 0; i < n; i++) {
    y[i] = a * x[i] + y[i];
  }

  elapsed += omp_get_wtime();
  printf("saxpy done in %6.3lf seconds.\n", elapsed);
}

int main() {
  int n = 2000000;
  float *x = (float*) malloc(n*sizeof(float));
  float *y = (float*) malloc(n*sizeof(float));
  float alpha = 2.0;

  #pragma omp parallel for
  for (int i = 0; i < n; i++) {
     x[i] = 1;
     y[i] = i;
  }

  saxpy(n, alpha, x, y);

  free(x);
  free(y);

  return 0;
}
