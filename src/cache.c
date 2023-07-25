#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <string.h>
#include "stats.h"
#include "cache.h"
#include "ll.h"

struct _Cache {
  List *buckets;
  LRUQueue queue;
  struct Stats text_stats, bin_stats;
  pthread_mutex_t *row_locks;
  pthread_mutex_t ts_lock, bs_lock;
  uint32_t nregions, size;
};

/* --------------- CACHE ------------------- */
#define NROW(val, len) (hash_bytes(val, len) % cache->size)
#define NREGION(idx) (idx % cache->nregions)
#define LOCK_ROW(idx) pthread_mutex_lock(cache->row_locks + NREGION(idx))
#define TRYLOCK_ROW(idx) pthread_mutex_trylock(cache->row_locks + NREGION(idx))
#define UNLOCK_ROW(idx) pthread_mutex_unlock(cache->row_locks + NREGION(idx))

Cache cache_init(uint64_t size, uint64_t nregions) {
  Cache cache = malloc(sizeof(struct _Cache));
  assert(cache);
  cache->buckets    = malloc(list_size() * size);
  cache->row_locks  = malloc(sizeof(pthread_mutex_t) * nregions);
  assert(cache->buckets && cache->row_locks);
  cache->queue  = queue_init();
  cache->text_stats = stats_init();
  cache->bin_stats  = stats_init();
  cache->nregions = nregions;
  cache->size = size;
  pthread_mutex_init(&cache->ts_lock, NULL);
  pthread_mutex_init(&cache->bs_lock, NULL);
  for (int i = 0; i < nregions; i++)
    pthread_mutex_init(cache->row_locks + i, NULL);
  return cache;
}

enum code cache_get(Cache cache, char mode, char* key, unsigned klen, char **val, unsigned *vlen) {
  unsigned idx = NROW(key, klen);
  LOCK_ROW(idx);
  List node = list_search(cache->buckets[idx], mode, key, klen);
  if (!node) {
    UNLOCK_ROW(idx);
    return EINVALID;
  }
  Data data = list_get_data(node);
  *vlen = data.vlen;
  *val = malloc(*vlen);
  assert(*val);
  memcpy(*val, data.val, *vlen); // Copiamos para proteger la informacion
  reset_lru_status(cache_get_lru_queue(cache), list_get_lru_priority(node));
  UNLOCK_ROW(idx);
  return OK;
}

Data data_wrap(char *key, unsigned klen, char *val, unsigned vlen, char mode) {
  Data data;
  data.key = key;
  data.val = val;
  data.klen = klen;
  data.vlen = vlen;
  data.mode = mode;
  return data;
}

enum code cache_put(Cache cache, char mode, char* key, unsigned klen, char *value, unsigned vlen) {
  unsigned idx = NROW(key, klen);
  LOCK_ROW(idx);
  int found = 0;
  List node = list_search(cache->buckets[idx], mode, key, klen);
  if (!node) {
    Data new_data = data_wrap(key, klen, value, vlen, mode);
    // unlock row hasta conseguir prioridad?
    List new_node = list_insert(cache->buckets[idx], new_data);
    LRUNode lru_priority = queue_push(cache->queue, idx, new_node);
    // aca habria que lockear la row y hicimos el unlock antes
    list_set_lru_priority(new_node, lru_priority);
  } else {
    node->data.mode = mode;
    free(node->data.val);
    node->data.val = value;
    node->data.vlen = vlen;
    reset_lru_status(cache->queue, node);
  }
  UNLOCK_ROW(idx);
  return OK;
}

enum code cache_del(Cache cache, char mode, char* key, unsigned klen) {
  unsigned idx = NROW(key, klen);
  LOCK_ROW(idx);
  List list = cache->buckets[idx];
  if (list_empty(list)) {
    UNLOCK_ROW(idx);
    return OK;
  }
  List del_node = list_search_and_remove(list, mode, key, klen);
  if(!del_node) {
    UNLOCK_ROW(idx);
    return EINVALID;
  }
  else {
    LRUNode lru_priority = list_get_lru_priority(del_node);
    queue_remove(cache->queue, lru_priority);
    UNLOCK_ROW(idx);
    ll_node_destroy(del_node);
  }
  return OK;
}

enum code cache_stats(Cache cache, char mode, struct Stats* stats) {
  switch (mode) {
  case TEXT_MODE:
    pthread_mutex_lock(&cache->ts_lock);
    *stats = cache->text_stats;
    pthread_mutex_unlock(&cache->ts_lock);
    break;
  case BIN_MODE:
    pthread_mutex_lock(&cache->bs_lock);
    *stats = cache->bin_stats;
    pthread_mutex_unlock(&cache->bs_lock);
    break;
  default:
    assert(0);
    return;
    break;
  }
  return OK;
}

LRUQueue cache_get_lru_queue(Cache cache) { return cache->queue; }

int cache_try_dismiss(Cache cache, uint64_t idx, List data_node) {
  if (!TRYLOCK_ROW(idx)) // Casilla bloqueada
    return 0;
  list_remove(data_node);
  UNLOCK_ROW(idx);
  return 1;
}
