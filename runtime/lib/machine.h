#ifndef _MACHINE_H_
#define _MACHINE_H_

#include <stdint.h>

#define INITIAL_ADDR_STACK_SIZE 1024
#define INITIAL_NODE_HEAP_SIZE 1024

extern int8_t **asp;

extern int8_t *nhp;
extern int8_t *nhp_max;

void minicute_machine_init(void);
#endif /* _MACHINE_H_ */
