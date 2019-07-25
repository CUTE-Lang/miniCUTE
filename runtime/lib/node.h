#ifndef _NODE_H_
#define _NODE_H_

#include <stdint.h>

typedef struct minicute_node_NInteger_body
{
  int32_t value;
} minicute_node_NInteger_body;

typedef struct minicute_node_NConstructor_body
{
  int32_t data_tag;
  int32_t data_arity;
} minicute_node_NConstructor_body;

typedef struct minicute_node_NStructure_body
{
  int32_t data_tag;
  int8_t** addresses;
} minicute_node_NStructure_body;

typedef struct minicute_node_NApplication_body
{
  int8_t* appliee;
  int8_t* applier;
} minicute_node_NApplication_body;

typedef struct minicute_node
{
  int8_t node_tag;
  union {
    minicute_node_NInteger_body node_integer_body;
    minicute_node_NConstructor_body node_constructor_body;
    minicute_node_NStructure_body node_structure_body;
    minicute_node_NApplication_body node_application_body;
  };
} minicute_node;

#endif
