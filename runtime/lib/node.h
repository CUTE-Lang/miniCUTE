/**
 * @file
 * @brief Defines Node Types and Node Utility Functions
 * @author Junyoung Clare Jang
 *
 * @details
 * ## Node Types
 * ### Kinds of Node Types
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
 * ### Restrictions for Node Types
 *   1. All value node types should be smaller than all updatable
 *      node types
 *
 * @attention
 * All variables/functions in this file should start with @c minicute__node__
 */
#ifndef _NODE_H_
#define _NODE_H_

#include <stdint.h>

/**
 * @name Node Tags
 * @anchor NodeTags
 * @todo Should it start with @c minicute__node__ ?
 * @{
 */
/**
 * @brief A Tag for #minicute__node__NEmpty
 */
#define NEmpty_TAG 0
/**
 * @brief A Tag for #minicute__node__NInteger
 */
#define NInteger_TAG 1
/**
 * @brief A Tag for #minicute__node__NConstructor
 */
#define NConstructor_TAG 2
/**
 * @brief A Tag for #minicute__node__NStructure
 */
#define NStructure_TAG 3
/**
 * @brief A Tag for #minicute__node__NApplication
 */
#define NApplication_TAG 4
/**
 * @brief A Tag for #minicute__node__NIndirect
 */
#define NIndirect_TAG 5
/**
 * @brief A Tag for #minicute__node__NGlobal
 */
#define NGlobal_TAG 6

/**
 * @brief An Impl-detail Tag for #minicute__node__NStructureFields
 */
#define NStructureFields_TAG -1
/**
 * @}
 */

/**
 * @name Abstract Node Type
 * @{
 */
/**
 * @interface minicute__node
 * @brief A Super Type for Nodes
 */
typedef struct minicute__node
{
  /**
   * @brief Tag For Node
   * @details This must be one of @ref NodeTags "the node tags"
   */
  int8_t tag;
} minicute__node;
/**
 * @}
 */

/**
 * @name Concrete Node Types
 * @{
 */
/**
 * @implements minicute__node
 */
typedef struct minicute__node__NEmpty
{
  /**
   * @copydoc minicute__node::tag
   * @details For NEmpty, this must be #NEmpty_TAG
   */
  int8_t tag;
} minicute__node__NEmpty;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NInteger
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NInteger (must be #NInteger_TAG)
   */
  int8_t tag;
  int32_t value;
} minicute__node__NInteger;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NConstructor
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NConstructor (must be #NConstructor_TAG)
   */
  int8_t tag;
  int32_t data_tag;
  int32_t data_arity;
} minicute__node__NConstructor;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NStructure
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NStructure (must be #NStructure_TAG)
   */
  int8_t tag;
  int32_t data_tag;
  int8_t *data_fields;
} minicute__node__NStructure;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NStructureFields
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NStructureFields (must be #NStructureFields_TAG)
   */
  int8_t tag;
  int32_t size;
  int8_t *values[0];
} minicute__node__NStructureFields;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NApplication
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NApplication (must be #NApplication_TAG)
   */
  int8_t tag;
  int8_t *function;
  int8_t *argument;
} minicute__node__NApplication;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NIndirect
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NIndirect (must be #NIndirect_TAG)
   */
  int8_t tag;
  int8_t *referee;
} minicute__node__NIndirect;

/**
 * @implements minicute__node
 */
typedef struct minicute__node__NGlobal
{
  /**
   * @copydoc minicute__node::tag
   * @details Tag for NGlobal (must be #NGlobal_TAG)
   */
  int8_t tag;
  int8_t *global_function;
  int32_t global_arity;
} minicute__node__NGlobal;
/**
 * @}
 */

/**
 * @name Node Creating Functions
 * @{
 */
int8_t *minicute__node__create_NEmpty(void);
int8_t *minicute__node__create_NInteger(int32_t value);
int8_t *minicute__node__create_NConstructor(int32_t data_tag, int32_t data_arity);
int8_t *minicute__node__create_NStructure(int32_t data_tag, int8_t *data_fields);
int8_t *minicute__node__create_NStructureFields(int32_t size, int8_t **values);
int8_t *minicute__node__create_NApplication(int8_t *function, int8_t *argument);
int8_t *minicute__node__create_NIndirect(int8_t *referee);
int8_t *minicute__node__create_NGlobal(int8_t *global_function, int32_t global_arity);
/**
 * @}
 */

/**
 * @name Node Updating Functions
 * @{
 */
void minicute__node__update_NInteger(int32_t value, int8_t *target);
void minicute__node__update_NConstructor(int32_t data_tag, int32_t data_arity, int8_t *target);
void minicute__node__update_NStructure(int32_t data_tag, int8_t *data_fields, int8_t *target);
void minicute__node__update_NApplication(int8_t *function, int8_t *argument, int8_t *target);
void minicute__node__update_NIndirect(int8_t *referee, int8_t *target);
void minicute__node__update_NGlobal(int8_t *global_function, int32_t global_arity, int8_t *target);
/**
 * @}
 */
#endif /* _NODE_H_ */
