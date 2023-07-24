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

enum code cache_put(Cache cache, char mode, char* key, unsigned klen, char *value, unsigned vlen) {
  unsigned idx = NROW(key, klen);
  LOCK_ROW(idx);
  int found = 0;
  List list;
  for (list = cache->buckets[idx]; list && !found; list = list->next)
    if (CMP_KEYS(key, klen, list->data.key, list->data.klen))
      found = 1;

  if (!found) {
    Data data = {.mode = mode, .key = key, .klen = klen, .value = value, .vlen = vlen};
    cache->buckets[idx] = list_add(cache->buckets[idx], data);
    cache->buckets[idx]->node = queue_push(cache->queue, cache->buckets[idx], idx);
  } else {
    list->data.mode = mode;
    free(list->data.value);
    list->data.value = value;
    list->data.vlen = vlen;
    reset_lru_status(cache, list->node);
  }
  UNLOCK_ROW(idx);
  return OK;
}

void cache_del(Cache cache, char mode, char* key, unsigned klen) {
  unsigned idx = NROW(key, klen);
  LOCK_ROW(idx);
  List list = cache->buckets[idx];
  if (list_empty(list)) {
    UNLOCK_ROW(idx);
    return OK;
  } else if (CMP_KEYS(key, klen, list->data.key, list->data.klen)) {
    cache->buckets[idx] = list->next;
    struct QNode *node = list->node;
    free(list->data.key);
    free(list->data.value);
    free(list);
    pthread_mutex_lock(&cache->lru_queue);
    queue_remove(cache->lru_queue, node);
    free(node);
    pthread_mutex_unlock(&cache->lru_queue);
  } else
    while (!list_empty(list->next))
      if (CMP_KEYS(key, klen, list->next->data.key, list->next->data.klen)) {
        list->next = list->next->next;
        struct QNode *node = list->next->node;
        free(list->next->data.key);
        free(list->next->data.value);
        free(list->next);
        pthread_mutex_lock(&cache->lru_queue);
        queue_remove(cache->lru_queue, node);
        pthread_mutex_unlock(&cache->lru_queue);
      }
  
  UNLOCK_ROW(idx);
  return OK;
}

struct Stats cache_stats(Cache cache, char mode) {
  struct Stats stats;
  switch (mode) {
  case TEXT_MODE:
    pthread_mutex_lock(&cache->ts_lock);
    stats = cache->text_stats;
    pthread_mutex_unlock(&cache->ts_lock);
    break;
  case BIN_MODE:
    pthread_mutex_lock(&cache->bs_lock);
    stats = cache->bin_stats;
    pthread_mutex_unlock(&cache->bs_lock);
    break;
  default:
    assert(0);
    break;
  }
  return stats;
}

LRUQueue cache_get_lru_queue(Cache cache) { return cache->queue; }

int cache_try_dismiss(Cache cache, uint64_t idx, List data_node) {
  if (!TRYLOCK_ROW(idx)) // Casilla bloqueada
    return 0;
  list_remove(data_node);
  UNLOCK_ROW(idx);
  return 1;
}
