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

int main(void) {
  printf("%d\n", __compli_entry());
  return 0;
}
