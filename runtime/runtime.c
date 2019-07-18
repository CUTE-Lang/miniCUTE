#include <stdlib.h>

#define INITIAL_ADDR_STACK_SIZE 1024
#define INITIAL_NODE_HEAP_SIZE 1024

void *asp;
void *nhp;
void minicute__user__defined__main();

void minicute_init();

int main(/* TODO: use argc and argv */)
{
  minicute_init();
  minicute__user__defined__main();

  return 0;
}

void minicute_init()
{
  asp = malloc(INITIAL_ADDR_STACK_SIZE * sizeof asp);
  nhp = malloc(INITIAL_NODE_HEAP_SIZE * sizeof nhp);
}
