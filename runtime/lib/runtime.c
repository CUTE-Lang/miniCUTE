#include <stdint.h>
#include <stdio.h>

#include "machine.h"
#include "runtime.h"

int main(/* TODO: use argc and argv */)
{
  minicute_machine_init();
  minicute_machine_run();

  return 0;
}

void minicute_llvm_pointer_debug(void *p)
{
  printf("pointer: %p\n", p);
}
