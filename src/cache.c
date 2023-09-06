#define _GNU_SOURCE // pthread_rwlock_t
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <string.h>
#include "common.h"
#include "stats.h"
#include "cache.h"
#include "ll.h"
#include "dalloc.h"

struct _Cache {
  List *buckets;
  LRUQueue queue;
  struct Stats text_stats, bin_stats;
  pthread_rwlock_t *row_locks;
  pthread_mutex_t ts_lock, bs_lock;
  uint64_t nregions, size;
};

LRUQueue cache_get_lru_queue(Cache cache) { return cache->queue; }

unsigned long hash_bytes(char *bytes, uint64_t nbytes) {
  unsigned long hashval, i;
  for (i = 0, hashval = 0; i < nbytes; ++i, bytes++)
    hashval = *bytes + 31 * hashval;
  return hashval;
}

/* --------------- CACHE ------------------- */
#define NROW(val, len) (hash_bytes(val, len) % cache->size)
#define NREGION(idx) (idx % cache->nregions)
#define RD_LOCK_ROW(idx) pthread_rwlock_rdlock(cache->row_locks + NREGION(idx))
#define RD_TRYLOCK_ROW(idx) pthread_rwlock_tryrdlock(cache->row_locks + NREGION(idx))
#define WR_LOCK_ROW(idx) pthread_rwlock_wrlock(cache->row_locks + NREGION(idx))
#define WR_TRYLOCK_ROW(idx) pthread_rwlock_trywrlock(cache->row_locks + NREGION(idx))
#define UNLOCK_ROW(idx) pthread_rwlock_unlock(cache->row_locks + NREGION(idx))

Cache cache_init(uint64_t size, uint64_t nregions) {
  Cache cache = malloc(sizeof(struct _Cache));
  assert(cache);
  cache->buckets    = malloc(sizeof(List) * size);
  cache->row_locks  = malloc(sizeof(pthread_rwlock_t) * nregions);
  assert(cache->buckets && cache->row_locks);
  cache->queue      = lru_init();
  cache->text_stats = stats_init();
  cache->bin_stats  = stats_init();
  cache->nregions   = nregions;
  cache->size       = size;

  if (pthread_mutex_init(&cache->ts_lock, NULL) < 0)
    quit("Inicializado lock para estadisticas de texto");
  if (pthread_mutex_init(&cache->bs_lock, NULL) < 0)
    quit("Inicializado lock para estadisticas binarias");
  for (uint32_t i = 0; i < nregions; i++)
    if (pthread_rwlock_init(cache->row_locks + i, NULL) < 0)
      quit("Inicializado de lock para region de cache");
  for (uint32_t i = 0; i < size; i++)
    cache->buckets[i] = list_init();
  // log(2, "Inicializado de cache con %lu casillas y %lu regiones", size, nregions);
  return cache;
}

void cache_destroy(Cache cache) {
  for (uint32_t i = 0; i < cache->nregions; i++)
    pthread_rwlock_destroy(cache->row_locks + i);
  pthread_mutex_destroy(&cache->ts_lock);
  pthread_mutex_destroy(&cache->bs_lock);
  lru_destroy(cache->queue);
  free(cache->row_locks);
  for (uint32_t i = 0; i < cache->size; i++)
    list_free(cache->buckets[i]);
  free(cache->buckets);
  free(cache);
}

void cache_update_stats(Cache cache, char mode, void (*update)(struct Stats*)) {
  pthread_mutex_t *lock;
  struct Stats *stats;
  if (mode == TEXT_MODE) {
    lock = &cache->ts_lock;
    stats = &cache->text_stats;
  } else if (mode == BIN_MODE) {
    lock = &cache->bs_lock;
    stats = &cache->bin_stats;
  } else
    assert(0);
  pthread_mutex_lock(lock);
  update(stats);
  pthread_mutex_unlock(lock);
}

enum code make_cache_request(Cache cache, enum code op, char mode, char *toks[2], uint32_t lens[2],
                             char **answer, uint32_t *ans_len) {
  const size_t stats_size = 2000;
  enum code res;
  switch (op) {
      case PUT:
        res = cache_put(cache, mode, toks[0], lens[0], toks[1], lens[1]);
        *answer = NULL;
        *ans_len = 0;
        break;

      case DEL:
        res = cache_del(cache, mode, toks[0], lens[0]);
        *answer = NULL;
        *ans_len = 0;
        break;

      case GET:
        res = cache_get(cache, mode, toks[0], lens[0], answer, ans_len);
        break;

      case STATS: ;
        struct Stats stats_buf = stats_init();
        res = cache_stats(cache, mode, &stats_buf);
        if (res == OK) {
          *answer = dalloc(stats_size);
          if (*answer == NULL)
            return EOOM;
          *ans_len = format_stats(&stats_buf, *answer, stats_size);
        } else {
          *ans_len = 0;
          *answer = NULL;
        }
        break;

      case EUNK:
        res = op;
        *answer = NULL;
        *ans_len = 0;
        break;
        
      case EINVALID:
        res = op;
        *answer = NULL;
        *ans_len = 0;
        break;

      default:
        assert(0);
    }

    return res;
}

enum code cache_get(Cache cache, char mode, char* key, unsigned klen, char **val, unsigned *vlen) {
  cache_update_stats(cache, mode, stats_inc_get);
  unsigned idx = NROW(key, klen);
  RD_LOCK_ROW(idx);
  List node = list_search(cache->buckets[idx], key, klen);
  if (!node) {
    UNLOCK_ROW(idx);
    *val = NULL;
    *vlen = 0;
    return ENOTFOUND;
  }
  Data data = list_get_data(node);
  if (mode == TEXT_MODE && data.mode == BIN_MODE) {
    UNLOCK_ROW(idx);
    *val = NULL;
    *vlen = 0;
    return EBINARY;
  }
  *vlen = data.vlen;
  *val = dalloc(*vlen);
  if (*val == NULL)
    return EOOM;
  memcpy(*val, data.val, *vlen); // Copiamos para proteger la informacion
  reset_lru_status(cache_get_lru_queue(cache), list_get_lru_priority(node));
  UNLOCK_ROW(idx);
  return OK;
}

enum code cache_put(Cache cache, char mode, char* key, unsigned klen, char *value, unsigned vlen) {
  cache_update_stats(cache, mode, stats_inc_put);
  unsigned idx = NROW(key, klen);
  WR_LOCK_ROW(idx);
  List node = list_search(cache->buckets[idx], key, klen);
  if (!node) {
    char *key_copy, *value_copy;
    if (!(key_copy = dalloc(klen))) {
      UNLOCK_ROW(idx);
      return EOOM;
    } else if (!(value_copy = dalloc(vlen))) {
      free(key_copy);
      UNLOCK_ROW(idx);
      return EOOM;
    }
    memcpy(key_copy, key, klen);
    memcpy(value_copy, value, vlen);
    Data new_data = data_wrap(key_copy, klen, value_copy, vlen, mode);
    List new_node = list_insert(cache->buckets[idx], new_data);
    if (!new_node) {
      free(key_copy);
      free(value_copy);
      UNLOCK_ROW(idx);
      return EOOM;
    }
    LRUNode lru_priority = lru_push(cache->queue, idx, new_node);
    list_set_lru_priority(new_node, lru_priority);
    cache_update_stats(cache, mode, stats_inc_keys);
  } else {
    char *value_copy;
    Data data = list_get_data(node);
    if (!(value_copy = dalloc(vlen))) {
      UNLOCK_ROW(idx);
      return EOOM;
    }
    free(data.val);
    memcpy(value_copy, value, vlen);
    data.val = value_copy;
    data.mode = mode;
    data.vlen = vlen;
    list_set_data(node, data);
    reset_lru_status(cache->queue, list_get_lru_priority(node));
  }
  UNLOCK_ROW(idx);
  return OK;
}

enum code cache_del(Cache cache, char mode, char* key, unsigned klen) {
  cache_update_stats(cache, mode, stats_inc_del);
  unsigned idx = NROW(key, klen);
  WR_LOCK_ROW(idx);
  List list = cache->buckets[idx];
  List del_node = list_search_and_remove(list, key, klen);
  if (!del_node) {
    UNLOCK_ROW(idx);
    return ENOTFOUND;
  } else {
    LRUNode lru_priority = list_get_lru_priority(del_node);
    lru_remove(cache->queue, lru_priority);
    UNLOCK_ROW(idx);
    lru_free_node(lru_priority);
    list_free_node(del_node);
    cache_update_stats(cache, mode, stats_dec_keys);
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
    return EUNK;
  }
  return OK;
}

int cache_try_dismiss(Cache cache, uint64_t idx, List data_node) {
  if (WR_TRYLOCK_ROW(idx) != 0) // Casilla bloqueada
    return 0;
  list_remove(data_node);
  list_free_node(data_node);
  UNLOCK_ROW(idx);
  return 1;
}
