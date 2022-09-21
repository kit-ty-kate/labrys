#include <stdint.h>
#include <stdlib.h>

void *GC_malloc(int32_t size) {
  return malloc(size);
}

void GC_param(void *ptr, void** env, int32_t env_size) {
}

void GC_return(void *ptr) {
}

void GC_register_local(void *ptr) {
}

void GC_register_global(void *ptr) {
}

void GC_init(int32_t initial_heap_size) {
}
