#include <string.h>
#include "dalloc.h"
#include "memcached.h"
#include "cache.h"
#include "lru.h"

void* dalloc(size_t size) {
  void* ptr = malloc(size);
  while (!ptr && !lru_empty(cache_get_lru_queue(cache))) {
    lru_dismiss(cache);
    log(2,"desalojo realizado!");
    ptr = malloc(size);
  }
  return ptr;
}

void* drealloc(void* ptr, size_t size, size_t inc) {
  void* realloc_ptr = dalloc(size + inc);
  memmove(realloc_ptr, ptr, size);
  free(ptr);
  return realloc_ptr;
}