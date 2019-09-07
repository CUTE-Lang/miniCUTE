/**
 * @file
 * @brief Variables/Functions for the Abstract Machine
 * @author Junyoung Clare Jang
 */
#ifndef _MACHINE_H_
#define _MACHINE_H_

#include <stdint.h>

/**
 * @brief Initial size for the address stack
 */
#define INITIAL_ADDR_STACK_SIZE 1024
/**
 * @brief Initial size for the node heap
 */
#define INITIAL_NODE_HEAP_SIZE 1024

/**
 * @brief Address Stack Pointer
 * @todo use register?
 */
extern int8_t **asp;
/**
 * @brief Address (Stack) Base Pointer
 * @todo use register?
 */
extern int8_t **abp;

/**
 * @brief Node Heap Pointer
 * @todo use register?
 */
extern int8_t *nhp;
/**
 * @brief Maximum Value of Node Heap Pointer
 * @todo use register?
 */
extern int8_t *nhp_max;

/**
 * @brief Function to initiate the machine
 */
void minicute__machine__init(void);
/**
 * @brief Function to execute the machine
 */
void minicute__machine__run(void);
#endif /* _MACHINE_H_ */
