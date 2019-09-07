/**
 * @file
 * @author Junyoung Clare Jang
 * @brief Defines Node Types and Node Utility Functions
 * # Node Types
 * ## Kinds of Node Types
 *   1. Classify by returnability
 *     - Value Nodes
 *       - NInteger
 *       - NConstructor
 *       - NStructure
 *       - NApplication
 *       - NIndirect
 *       - NGlobal
 *     - Non-value Nodes
 *       - NEmpty
 *   2. Classify by updatability
 *     - Updatable Nodes
 *       - NConstructor
 *       - NApplication
 *       - NGlobal
 *     - Constant Nodes
 *       - NEmpty
 *       - NInteger
 *       - NStructure
 *       - NIndirect
 *
 * ## Restrictions for Node Types
 *   1. All value node types should be smaller than all updatable
 *      node types
 */
#ifndef _NODE_H_
#define _NODE_H_

#include <stdint.h>

/**
 * @name Node Tags
 */
/// @{
#define NEmpty_TAG 0
#define NInteger_TAG 1
#define NConstructor_TAG 2
#define NStructure_TAG 3
#define NApplication_TAG 4
#define NIndirect_TAG 5
#define NGlobal_TAG 6

/**
 * @brief An Impl-detail Subnode Tag for NStructure Node
 */
#define NStructureFields_TAG -1
/// @}

/**
 * @brief Super type for all nodes
 */
typedef struct minicute__node
{
  int8_t tag;
} minicute__node;

typedef struct minicute__node__NEmpty
{
  int8_t tag;
} minicute__node__NEmpty;

typedef struct minicute__node__NInteger
{
  int8_t tag;
  int32_t value;
} minicute__node__NInteger;

typedef struct minicute__node__NConstructor
{
  int8_t tag;
  int32_t data_tag;
  int32_t data_arity;
} minicute__node__NConstructor;

typedef struct minicute__node__NStructure
{
  int8_t tag;
  int32_t data_tag;
  int8_t *data_fields;
} minicute__node__NStructure;

typedef struct minicute__node__NStructureFields
{
  int8_t tag;
  int32_t size;
  int8_t *values[0];
} minicute__node__NStructureFields;

typedef struct minicute__node__NApplication
{
  int8_t tag;
  int8_t *function;
  int8_t *argument;
} minicute__node__NApplication;

typedef struct minicute__node__NIndirect
{
  int8_t tag;
  int8_t *referee;
} minicute__node__NIndirect;

typedef struct minicute__node__NGlobal
{
  int8_t tag;
  int8_t *global_function;
  int32_t global_arity;
} minicute__node__NGlobal;

/**
 * @name Node Creating Functions
 */
/// @{
int8_t *minicute__node__create_NEmpty(void);
int8_t *minicute__node__create_NInteger(int32_t value);
int8_t *minicute__node__create_NConstructor(int32_t data_tag, int32_t data_arity);
int8_t *minicute__node__create_NStructure(int32_t data_tag, int8_t *data_fields);
int8_t *minicute__node__create_NStructureFields(int32_t size, int8_t **values);
int8_t *minicute__node__create_NApplication(int8_t *function, int8_t *argument);
int8_t *minicute__node__create_NIndirect(int8_t *referee);
int8_t *minicute__node__create_NGlobal(int8_t *global_function, int32_t global_arity);
/// @}

/**
 * @name Node Updating Functions
 */
/// @{
void minicute__node__update_NInteger(int32_t value, int8_t *target);
void minicute__node__update_NConstructor(int32_t data_tag, int32_t data_arity, int8_t *target);
void minicute__node__update_NStructure(int32_t data_tag, int8_t *data_fields, int8_t *target);
void minicute__node__update_NApplication(int8_t *function, int8_t *argument, int8_t *target);
void minicute__node__update_NIndirect(int8_t *referee, int8_t *target);
void minicute__node__update_NGlobal(int8_t *global_function, int32_t global_arity, int8_t *target);
/// @}
#endif /* _NODE_H_ */
