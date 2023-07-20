#include <stdlib.h>
#include <assert.h>
#include "memcached.h"
#include "cache.h"
#include "hash.h"
#include "queue.h"

#define LRU_FREE_SIZE 10 // Cantidad de elementos a desalojar cuando falta memoria

struct _Cache {
  HashTable table;
  Queue lru_queue;
  struct Stats text_stats, bin_stats;
};

Cache cache_init(unsigned long size) {
  Cache cache = malloc(sizeof(struct _Cache));
  assert(cache);
  cache->table      = hashtable_init(size);
  cache->lru_queue  = queue_init();
  cache->text_stats = stats_init();
  cache->bin_stats  = stats_init();
  return cache;
}

char* cache_get(Cache cache, char mode, char* key, unsigned klen, unsigned *vlen) {
  
}

void cache_put(Cache cache, char mode, char* key, unsigned klen, char *value, unsigned vlen);

void cache_del(Cache cache, char mode, char* key, unsigned len);

struct Stats cache_stats(Cache cache, char mode);

void cache_free_lru(Cache cache);
