#ifndef _NODE_H_
#define _NODE_H_

#include <stdint.h>

#define NEmpty_TAG 0
#define NInteger_TAG 1
#define NConstructor_TAG 2
#define NStructure_TAG 3
#define NApplication_TAG 4
#define NIndirect_TAG 5
#define NGlobal_TAG 6

#define NStructureFields_TAG -1

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
  int8_t *data_fields;
} minicute_node_NStructure;

typedef struct minicute_node_NStructureFields
{
  int8_t tag;
  int32_t size;
  int8_t *values;
} minicute_node_NStructureFields;

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

typedef struct minicute_node_NGlobal
{
  int8_t tag;
  int8_t *global_function;
  int32_t global_arity;
} minicute_node_NGlobal;

int8_t *minicute_create_node_NEmpty(void);
int8_t *minicute_create_node_NInteger(int32_t value);
int8_t *minicute_create_node_NConstructor(int32_t data_tag, int32_t data_arity);
int8_t *minicute_create_node_NStructure(int32_t data_tag, int8_t *data_fields);
int8_t *minicute_create_node_NStructureFields(int32_t size, int8_t *values);
int8_t *minicute_create_node_NApplication(int8_t *function, int8_t *argument);
int8_t *minicute_create_node_NIndirect(int8_t *referee);
int8_t *minicute_create_node_NGlobal(int8_t *global_function, int32_t global_arity);
#endif /* _NODE_H_ */
