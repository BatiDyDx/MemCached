#include "dalloc.h"
#include "memcached.h"

void* dalloc(size_t size) {
  void* ptr = malloc(size);
  for (int i = 0; i < DISMISS_ATTEMPTS && !ptr; i++) {
    lru_dismiss(cache);
    ptr = malloc(size);
  }
  return ptr;
}
