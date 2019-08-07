#ifndef _MACHINE_H_
#define _MACHINE_H_
/**
 * @file
 * @author Junyoung Clare Jang
 * @brief Variables/Functions for the Abstract Machine
 */

#include <stdint.h>

#define INITIAL_ADDR_STACK_SIZE 1024
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

void minicute_machine_init(void);
void minicute_machine_run(void);
#endif /* _MACHINE_H_ */
