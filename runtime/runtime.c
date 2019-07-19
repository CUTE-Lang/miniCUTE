#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include "runtime.h"

int8_t **asp;

int8_t *nhp;
int8_t *nhp_max;

int main(/* TODO: use argc and argv */)
{
  minicute_init();

  /**
   * Entry point of a miniCUTE module
   */
  minicute__user__defined__main();

  return 0;
}

void minicute_init()
{
  asp = malloc(INITIAL_ADDR_STACK_SIZE * sizeof asp);
  minicute_llvm_pointer_debug(asp);
  nhp = malloc(INITIAL_NODE_HEAP_SIZE * sizeof nhp);
  nhp_max = nhp + INITIAL_NODE_HEAP_SIZE;
}

void minicute_llvm_pointer_debug(void *p)
{
  printf("pointer: %p\n", p);
}
