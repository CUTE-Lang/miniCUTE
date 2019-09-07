#include <stdint.h>
#include <stdio.h>

#include "machine.h"

int main(/* TODO: use argc and argv */)
{
  minicute__machine__init();
  minicute__machine__run();

  return 0;
}
