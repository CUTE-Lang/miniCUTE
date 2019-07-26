#include "machine.h"
#include "node.h"

#define ALLOC_NODE(type, name)                  \
  minicute_node_##type *name = (void *) nhp;    \
  nhp += sizeof(*name);                         \
  node->tag = type##_TAG;

__attribute__((always_inline))
int8_t *minicute_create_node_NEmpty(void)
{
  ALLOC_NODE(NEmpty, node);

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NInteger(int32_t value)
{
  ALLOC_NODE(NInteger, node);
  node->value = value;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NConstructor(int32_t data_tag, int32_t data_arity)
{
  ALLOC_NODE(NConstructor, node);
  node->data_tag = data_tag;
  node->data_arity = data_arity;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NStructure(int32_t data_tag, int32_t data_arity, int8_t **data_fields)
{
  ALLOC_NODE(NStructure, node);
  node->data_tag = data_tag;
  node->data_arity = data_arity;
  node->data_fields = data_fields;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NApplication(int8_t *function, int8_t *argument)
{
  ALLOC_NODE(NApplication, node);
  node->function = function;
  node->argument = argument;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NIndirect(int8_t *referee)
{
  ALLOC_NODE(NIndirect, node);
  node->referee = referee;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NGlobal(int8_t *global_function, int32_t global_arity)
{
  ALLOC_NODE(NGlobal, node);
  node->global_function = global_function;
  node->global_arity = global_arity;

  return (void *) node;
}
