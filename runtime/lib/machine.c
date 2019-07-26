#include <stdlib.h>

#include "machine.h"

int8_t **asp;

int8_t *nhp;
int8_t *nhp_max;

void minicute_machine_init(void)
{
  asp = malloc(INITIAL_ADDR_STACK_SIZE * sizeof asp);
  nhp = malloc(INITIAL_NODE_HEAP_SIZE * sizeof nhp);
  nhp_max = nhp + INITIAL_NODE_HEAP_SIZE;
}
