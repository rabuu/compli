#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

extern int32_t __compli_entry();

int32_t __compli_trace_int(int32_t x) {
  printf("%d\n", x);
  return x;
}

float __compli_trace_float(float x) {
  printf("%f\n", x);
  return x;
}

bool __compli_trace_bool(bool x) {
  x ? printf("true\n") : printf("false\n");
  return x;
}

int32_t __compli_bool_to_int(bool x) {
  return (int32_t) x;
}

int32_t __compli_float_to_int(float x) {
  return (int32_t) x;
}

float __compli_int_to_float(int32_t x) {
  return (float) x;
}

// square root approximation to avoid math.h dependency
float __compli_sqrt(float x) {
  const float MINDIFF = 2.25e-308;

  float root=x/3, last, diff=1;
  if (x <= 0) return 0;
  do {
    last = root;
    root = (root + x / root) / 2;
    diff = root - last;
  } while (diff > MINDIFF || diff < -MINDIFF);
  return root;
}

int main(void) {
  printf("%d\n", __compli_entry());
  return 0;
}
