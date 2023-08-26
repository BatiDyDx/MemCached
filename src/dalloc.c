#include "dalloc.h"
#include "memcached.h"

void* dalloc(size_t size) {
  void* ptr = malloc(size);
  while (!ptr && !lru_empty(cache_get_lru_queue(cache))) {
    lru_dismiss(cache);
    ptr = malloc(size);
  }
  return ptr;
}
