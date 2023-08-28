#ifndef __DALLOC_H__
#define __DALLOC_H__

#include <stdlib.h>
#include "cache.h"
#include "lru.h"

#define DISMISS_ATTEMPTS 15

void* dalloc(size_t size);

void* drealloc(void* ptr, size_t size, size_t inc);

#endif