#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include "stats.h"
#include "cache.h"
#include "hash.h"
#include "ll.h"

#define LRU_FREE_SIZE 10 // Cantidad de elementos a desalojar cuando falta memoria

struct _Cache {
  List *buckets;
  Queue lru_queue;
  struct Stats text_stats, bin_stats;
  pthread_mutex_t *row_locks;
  pthread_mutex_t lru_lock;
  pthread_spinlock_t ts_lock, bs_lock;
  uint32_t nregions, size;
};

/* ----------------- Colas  ---------------- */
typedef struct _Data {
  char *key, *value;
  unsigned klen, vlen;
  char mode; // TEXT_MODE o BIN_MODE
} Data;

struct QNode {
  struct QNode *prev, *next;
  uint64_t idx;
  Data data;
};

typedef struct _Queue {
  struct QNode *first, *last;
  unsigned long len;
} *Queue;

Queue queue_init() {
  Queue q = malloc(sizeof(struct _Queue));
  assert(q);
  q->first = q->last = NULL;
  q->len = 0;
  return q;
}

void queue_free(Queue q) {
  while (!queue_empty(q))
    queue_pop(q);
}

int queue_empty(Queue q) { return q->len == 0; }

void queue_push(Queue q, Data data) {
  struct QNode *new_node = malloc(sizeof(struct QNode));
  assert(new_node);
  new_node->data = data;
  q->last = new_node;
  
  if (queue_empty(q))
    q->first = new_node;
  else
    q->last->next = new_node;

  new_node->prev = q->last;
  new_node->next = NULL;
  q->len++;
  return new_node;
}

// Podemos asumir colas no vacias
Data queue_start(Queue q) {
  return q->first->data;
}

void queue_pop(Queue q) {
  if (queue_empty(q))
    return;
  struct QNode *node = q->first;
  if (--q->len == 0)
    q->last = NULL;
  q->first = node->next;
  q->first->prev = NULL;
  free(node);
}

void queue_remove(Queue q, struct QNode *node) {
  if (q->first == node)
    q->first = node->next;
  else
    node->prev->next = node->next;
  if (q->last == node)
    q->last = node->prev;
  else
    node->next->prev = node->prev;
}

/* ----------------------------------------- */
/* ---------------- TABLAS ----------------- */

/* Listas enlazadas simples - Dedicadas a uso interno de la tabla hash */

struct _LLNode {
  Data data;
  struct QNode *node; // Nodo de la cola, no confundir con el siguiente de la lista
  struct _LLNode* next;
};

typedef struct _LLNode *List;

List list_init() { return NULL; }

void list_free(List list) {
  while (list != NULL) {
    list = list_remove_first(list);
  }
}

int list_empty(List list) { return list == NULL; }

List list_add(List list, struct QNode *node) {
  List new_node = malloc(sizeof(struct _LLNode));
  assert(new_node != NULL);
  new_node->next = list;
  new_node->node = node;
  return new_node;
}

List list_remove_first(List list) {
  List pnode;
  if (list_empty(list))
      return NULL;
  pnode = list;
  list = list->next;
  free(pnode);
  return list;
}
/* -------------------------------------------------- */

#define CMP_KEYS(K1, LEN1, K2, LEN2) (LEN1 == LEN2 && memcmp(K1, K2, LEN1))

/* Tabla hash implementada con encadenamiento externo */
struct _HashTable {
  List *elems;
  unsigned nregions;
  pthread_mutex_t *region_locks;
  unsigned size;
};

/**
 * Funcion de hash para strings propuesta por Kernighan & Ritchie en "The C
 * Programming Language (Second Ed.)".
*/
unsigned long hash_bytes(char *bytes, unsigned long nbytes) {
  unsigned long hashval, i;
  for (i = 0, hashval = 0; i < nbytes; ++i, bytes++)
    hashval = *bytes + 31 * hashval;
  return hashval;
}

HashTable hashtable_init(unsigned size, unsigned nregions) {
  HashTable table = malloc(sizeof(struct _HashTable));
  assert(table != NULL);
  // We ask for 0 initialized memory (every element is a NULL pointer)
  table->elems = calloc(size, sizeof(List));
  assert(table->elems != NULL);
  table->nregions = nregions;
  table->size = size;
  table->region_locks = malloc(sizeof(pthread_mutex_t) * nregions);
  if (!table->region_locks)
    quit("malloc de locks para tabla hash");
  for (int i = 0; i < nregions; i++)
    pthread_mutex_init(table->region_locks + i, NULL);
  return table;
}

unsigned hashtable_size(HashTable table) { return table->size; }

void hashtable_free(HashTable table) {
  for (unsigned idx = 0; idx < table->size; ++idx)
    list_free(table->elems[idx]);
  for (int i = 0; i < table->nregions; i++)
    pthread_mutex_destroy(table->region_locks + i);
  free(table->elems);
  free(table);
}

// Esta funcion asume que la llave no se encuentra repetida
// Esto es garantizado por la implementacion de cache
void hashtable_insert(HashTable table, struct QNode *node) {
  // TODO - Posible optimizacion: La interfaz de cache busca el nodo y si no esta
  // lo inserta, esto lleva a caclular dos veces el hash
  unsigned idx = hash_bytes(node->data.key, node->data.klen) % table->size;
  pthread_mutex_t *lock = table->region_locks + (idx % table->nregions);
  pthread_mutex_lock(lock);
  table->elems[idx] = list_add(table->elems[idx], node);
  pthread_mutex_unlock(lock);
}

struct QNode* hashtable_search(HashTable table, char* key, unsigned klen) {
  int found = 0;
  List list;
  unsigned idx = hash_bytes(key, klen) % table->size;
  pthread_mutex_t *lock = table->region_locks + (idx % table->nregions);
  pthread_mutex_lock(lock);
  list = table->elems[idx];
  while (list && !found) {
    if (CMP_KEYS(key, klen, list->node->data.key, list->node->data.klen))
      found = 1;
    else
      list = list->next;
  }
  pthread_mutex_unlock(lock);
  return list;
}

int hashtable_remove(HashTable table, char *key, unsigned klen) {
  unsigned idx = hash_bytes(key, klen) % table->size;
  pthread_mutex_t *lock = table->region_locks + (idx % table->nregions);
  pthread_mutex_lock(lock);
  List list = table->elems[idx];
  if (list_empty(list)) {
    pthread_mutex_unlock(lock);
    return 0;
  } else if (CMP_KEYS(key, klen, list->node->data.key, list->node->data.klen)) {
    table->elems[idx] = list_remove_start(list);
    pthread_mutex_unlock(lock);
    return 1;
  }
  while (!list_empty(list->next)) {
    if (CMP_KEYS(key, klen, list->next->node->data.key, list->next->node->data.klen)) {
      List tmp = list->next;
      list->next = tmp->next;
      free(tmp);
      pthread_mutex_unlock(lock);
      return 1;
    }
  }
  pthread_mutex_unlock(lock);
  return 0;
}
/* ----------------------------------------- */

/* --------------- CACHE ------------------- */
#define NROW(val, len) (hash_bytes(val, len) % cache->size)
#define NREGION(idx) (idx % cache->nregions)
#define LOCK_ROW(idx) pthread_mutex_lock(cache->row_locks + NREGION(idx))
#define TRYLOCK_ROW(idx) pthread_mutex_trylock(cache->row_locks + NREGION(idx))
#define UNLOCK_ROW(idx) pthread_mutex_unlock(cache->row_locks + NREGION(idx))

Cache cache_init(uint64_t size, uint64_t nregions) {
  Cache cache = malloc(sizeof(struct _Cache));
  assert(cache);
  cache->buckets    = malloc(sizeof(struct _LLNode) * size);
  cache->row_locks  = malloc(sizeof(pthread_mutex_t) * nregions);
  assert(cache->buckets && cache->row_locks);
  cache->lru_queue  = queue_init();
  cache->text_stats = stats_init();
  cache->bin_stats  = stats_init();
  cache->nregions = nregions;
  cache->size = size;
  pthread_mutex_init(&cache->lru_lock, NULL);
  pthread_spinlock_init(&cache->ts_lock, NULL);
  pthread_spinlock_init(&cache->bs_lock, NULL);
  for (int i = 0; i < nregions; i++)
    pthread_mutex_init(cache->row_locks + i, NULL);
  return cache;
}

enum code cache_get(Cache cache, char mode, char* key, unsigned klen, char **val, unsigned *vlen) {
  unsigned idx = NROW(key, klen);
  LOCK_ROW(idx);

  int found = 0;
  List list;
  for (list = cache->buckets[idx]; list && !found; list = list->next)
    if (CMP_KEYS(key, klen, list->data.key, list->data.klen))
      found = 1;

  if (!found) {
    UNLOCK_ROW(idx);
    *vlen = 0;
    *val = NULL;
    return ENOTFOUND;
  }
  // Faltaria chequear si el modo en que se pide es igual al modo en que se almaceno
  // y de ahi implementar la forma de respuesta decidida
  *vlen = list->data.vlen;
  *val = list->data.value;
  reset_lru_status(cache, list->node);
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
    pthread_mutex_lock(&cache->lru_lock);
    queue_push(cache->lru_queue, cache->buckets[idx], idx);
    cache->buckets[idx]->node = cache->lru_queue->last;
    pthread_mutex_unlock(&cache->lru_lock);
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

int cache_free_lru(Cache cache) {
  int i;
  pthread_mutex_lock(&cache->lru_lock);
  struct QNode *node = cache->lru_queue->first;
  for (i = 0; i < LRU_FREE_SIZE && node; node = node->next) {
    if (!TRYLOCK_ROW(node->idx)) // Casilla bloqueada
      continue;
    queue_remove(cache->lru_queue, node);
    
    UNLOCK_ROW(node->idx);
    i++;
  }
  pthread_mutex_unlock(&cache->lru_lock);
  return i;
}

int reset_lru_status(Cache cache, struct QNode *node) {
  pthread_mutex_lock(&cache->lru_lock);
  Queue q = cache->lru_queue;
  queue_remove(q, node);

  if (queue_empty(q))
    q->first = node;
  else
    q->last->next = node;
  node->prev = q->last;
  node->next = NULL;
  pthread_mutex_unlock(&cache->lru_lock);
}
