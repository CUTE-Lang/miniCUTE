#ifndef _NODE_H_
#define _NODE_H_

#include <stdint.h>

#define N_EMPTY_TAG 0
#define N_INTEGER_TAG 1
#define N_CONSTRUCTOR_TAG 2
#define N_STRUCTURE_TAG 3
#define N_APPLICATION_TAG 4
#define N_INDIRECT_TAG 5
#define N_GLOBAL_TAG 6

typedef struct minicute_node
{
  int8_t tag;
} minicute_node;

typedef struct minicute_node_NEmpty
{
  int8_t tag;
} minicute_node_NEmpty;

typedef struct minicute_node_NInteger
{
  int8_t tag;
  int32_t value;
} minicute_node_NInteger;

typedef struct minicute_node_NConstructor
{
  int8_t tag;
  int32_t data_tag;
  int32_t data_arity;
} minicute_node_NConstructor;

typedef struct minicute_node_NStructure
{
  int8_t tag;
  int32_t data_tag;
  int32_t data_arity;
  int8_t **data_fields;
} minicute_node_NStructure;

typedef struct minicute_node_NApplication
{
  int8_t tag;
  int8_t *function;
  int8_t *argument;
} minicute_node_NApplication;

typedef struct minicute_node_NIndirect
{
  int8_t tag;
  int8_t *referee;
} minicute_node_NIndirect;

int8_t *minicute_create_node_NInteger(int32_t value);

#endif
