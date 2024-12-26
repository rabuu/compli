#include <stdio.h>
#include <stdint.h>

extern int32_t myMain();

int main(int argc, char** argv) {
  printf("%d\n", myMain());
  return 0;
}


