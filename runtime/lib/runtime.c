#include <stdint.h>
#include <stdio.h>

#include "node.h"
#include "machine.h"
#include "runtime.h"
#include "user.h"

int main(/* TODO: use argc and argv */)
{
  minicute_machine_init();

  /**
   * Entry point of a miniCUTE module
   */
  minicute__user__defined__main();

  return 0;
}

void minicute_llvm_pointer_debug(void *p)
{
  printf("pointer: %p\n", p);
}
