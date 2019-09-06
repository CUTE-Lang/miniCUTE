#include <stddef.h>

#include "machine.h"
#include "node.h"

#define ALLOC_NODE(type, name)                  \
  minicute_node_##type *name = (void *) nhp;    \
  nhp += sizeof(*name);                         \
  node->tag = type##_TAG

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
int8_t *minicute_create_node_NStructure(int32_t data_tag, int8_t *data_fields)
{
  ALLOC_NODE(NStructure, node);
  node->data_tag = data_tag;
  node->data_fields = data_fields;

  return (void *) node;
}

__attribute__((always_inline))
int8_t *minicute_create_node_NStructureFields(int32_t size, int8_t **values)
{
  minicute_node_NStructureFields *node = (void *) nhp;
  nhp += sizeof(*node) + sizeof(node->values[0]) * size;
  node->size = size;

  for (int i = 0; i < size; i ++)
  {
    node->values[i] = values[i];
  }

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


#define REPLACE_NODE(type, name, target)        \
  minicute_node_##type *name = (void *) target; \
  node->tag = type##_TAG

__attribute__((always_inline))
void minicute_update_node_NInteger(int32_t value, int8_t *target)
{
  REPLACE_NODE(NInteger, node, target);
  node->value = value;
}

__attribute__((always_inline))
void minicute_update_node_NConstructor(int32_t data_tag, int32_t data_arity, int8_t *target)
{
  REPLACE_NODE(NConstructor, node, target);
  node->data_tag = data_tag;
  node->data_arity = data_arity;
}

__attribute__((always_inline))
void minicute_update_node_NStructure(int32_t data_tag, int8_t *data_fields, int8_t *target)
{
  REPLACE_NODE(NStructure, node, target);
  node->data_tag = data_tag;
  node->data_fields = data_fields;
}

__attribute__((always_inline))
void minicute_update_node_NApplication(int8_t *function, int8_t *argument, int8_t *target)
{
  REPLACE_NODE(NApplication, node, target);
  node->function = function;
  node->argument = argument;
}

__attribute__((always_inline))
void minicute_update_node_NIndirect(int8_t *referee, int8_t *target)
{
  REPLACE_NODE(NIndirect, node, target);
  node->referee = referee;
}

__attribute__((always_inline))
void minicute_update_node_NGlobal(int8_t *global_function, int32_t global_arity, int8_t *target)
{
  REPLACE_NODE(NGlobal, node, target);
  node->global_function = global_function;
  node->global_arity = global_arity;
}

