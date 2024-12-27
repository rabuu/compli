#include <stdio.h>
#include <stdint.h>

extern int32_t __compli_entry();

int main(int argc, char** argv) {
  printf("%d\n", __compli_entry());
  return 0;
}


