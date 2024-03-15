#include <stdlib.h>
#include <string.h>
#include "dalloc.h"
#include "memcached.h"
#include "cache.h"
#include "lru.h"

void* dalloc(size_t size) {
  void* ptr;
  log(4, "Pedido de memoria de %u bytes", size);
#ifdef DEBUG_DALLOC // Emulate random memory error allocations
  if (rand() % 15 == 0)
    ptr = NULL;
  else
    ptr = malloc(size);
#else
  ptr = malloc(size);
#endif
  for (int i = 0;
      i < 10 && !ptr && !lru_empty(cache_get_lru_queue(cache));
      i++) {
    lru_dismiss(cache);
    ptr = malloc(size);
  }
  if (!ptr)
    log(1, "Muchos intentos de desalojo realizados, imposible satisfacer alocacion de memoria");
  return ptr;
}

void* drealloc(void* ptr, size_t size, size_t inc) {
  void* realloc_ptr = dalloc(size + inc);
  memmove(realloc_ptr, ptr, size);
  free(ptr);
  return realloc_ptr;
}
