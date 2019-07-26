#include "machine.h"
#include "node.h"

__attribute__((always_inline))
int8_t *minicute_create_node_NEmpty(void)
{
  minicute_node_NEmpty *node = (void *)nhp;
  nhp += sizeof(*node);

  node->tag = N_EMPTY_TAG;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NInteger(int32_t value)
{
  minicute_node_NInteger *node = (void *)nhp;
  nhp += sizeof(*node);

  node->tag = N_INTEGER_TAG;
  node->value = value;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NConstructor(int32_t data_tag, int32_t data_arity)
{
  minicute_node_NConstructor *node = (void *)nhp;
  nhp += sizeof(*node);

  node->tag = N_CONSTRUCTOR_TAG;
  node->data_tag = data_tag;
  node->data_arity = data_arity;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NStructure(int32_t data_tag, int32_t data_arity, int8_t **data_fields)
{
  minicute_node_NStructure *node = (void *)nhp;
  nhp += sizeof(*node);

  node->tag = N_STRUCTURE_TAG;
  node->data_tag = data_tag;
  node->data_arity = data_arity;
  node->data_fields = data_fields;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NApplication(int8_t *function, int8_t *argument)
{
  minicute_node_NApplication *node = (void *)nhp;
  nhp += sizeof(*node);

  node->tag = N_APPLICATION_TAG;
  node->function = function;
  node->argument = argument;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NIndirect(int8_t *referee)
{
  minicute_node_NIndirect *node = (void *)nhp;
  nhp += sizeof(*node);

  node->tag = N_INDIRECT_TAG;
  node->referee = referee;

  return (void *) node;
}

