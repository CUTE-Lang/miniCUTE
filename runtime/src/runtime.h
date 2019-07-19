#ifndef _RUNTIME_H_
#define _RUNTIME_H_

#include <stdint.h>

#define INITIAL_ADDR_STACK_SIZE 1024
#define INITIAL_NODE_HEAP_SIZE 1024

extern int8_t **asp;

extern int8_t *nhp;
extern int8_t *nhp_max;

void minicute__user__defined__main();

void minicute_init();

void minicute_llvm_pointer_debug(void *p);
#endif
