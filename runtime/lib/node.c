#include "machine.h"
#include "node.h"

__attribute__((always_inline))
int8_t *minicute_create_node_NInteger(int32_t value)
{
  minicute_node_NInteger *node = (void *)nhp;
  nhp += sizeof(minicute_node);

  node->tag = 0;
  node->value = value;

  return (void *) node;
}
