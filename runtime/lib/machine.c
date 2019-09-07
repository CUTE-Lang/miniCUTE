#include <stdlib.h>

#include "machine.h"
#include "node.h"
#include "user.h"

int8_t **asp;
int8_t **abp;

int8_t *nhp;
int8_t *nhp_max;

void minicute__machine__init(void)
{
  asp = malloc(INITIAL_ADDR_STACK_SIZE * sizeof asp);
  abp = asp;
  nhp = malloc(INITIAL_NODE_HEAP_SIZE * sizeof nhp);
  nhp_max = nhp + INITIAL_NODE_HEAP_SIZE;
}

void minicute__machine__run(void)
{
  asp[1] = (void *) &minicute__user_defined__main_node;
  asp += 1;
  ((void (*)(void))(void *)minicute__user_defined__main_node.global_function)();
}
