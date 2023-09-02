#define _GNU_SOURCE
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <pthread.h>
#include "cache.h"
#include "dalloc.h"
#include "lru.h"
#include "ll.h"

struct _LRUNode {
  unsigned idx;
  List data_node;
  struct _LRUNode *prev, *next;
};

struct _LRUQueue {
  pthread_mutex_t lock;
  struct _LRUNode *first, *last;
};

void lru_free_node(LRUNode node) {
  free(node);
}

static inline void lru_lock(LRUQueue q) {
  if (pthread_mutex_lock(&q->lock) < 0)
    quit("lru_lock");
}

static inline void lru_unlock(LRUQueue q) {
  if (pthread_mutex_unlock(&q->lock) < 0)
    quit("lru_unlock");
}

LRUQueue lru_init() {
  LRUQueue q = malloc(sizeof(struct _LRUQueue));
  assert(q);
  q->first = q->last = NULL;
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  assert(pthread_mutex_init(&q->lock, &attr) == 0);
  pthread_mutexattr_destroy(&attr);
  return q;
}

void lru_destroy(LRUQueue q) {
  LRUNode node = q->first;
  while (node) {
    LRUNode next = node->next;
    free(node);
    node = next;
  }
  pthread_mutex_destroy(&q->lock);
  free(q);
}

int lru_empty(LRUQueue q) {
  int b;
  lru_lock(q);
  b = !q->first;
  lru_unlock(q);
  return b;
}

LRUNode lru_push(LRUQueue q, unsigned idx, List data_node) {
  LRUNode new_node = dalloc(sizeof(struct _LRUNode));
  if (!new_node)
    return NULL;
  new_node->idx = idx;
  new_node->data_node = data_node;
  
  lru_lock(q);
  if (lru_empty(q))
    q->first = new_node;
  else
    q->last->next = new_node;
  q->last = new_node;

  new_node->prev = q->last;
  new_node->next = NULL;
  lru_unlock(q);
  return new_node;
}

void lru_remove(LRUQueue q, LRUNode node) {
  lru_lock(q);
  if (q->first == node) {
    q->first = node->next;
    if (node->next)
      node->next->prev = NULL;
  }
  else
    node->prev->next = node->next;
  if (q->last == node) {
    q->last = node->prev;
    if (node->prev)
      node->prev->next = NULL;
  }
  else {
    assert(node->next);
    //assert(node->prev);
    node->next->prev = node->prev;
  }
  lru_unlock(q);
}

void reset_lru_status(LRUQueue q, LRUNode node) {
  lru_lock(q);
  lru_remove(q, node);

  if (lru_empty(q))
    q->first = node;
  else
    q->last->next = node;
  node->prev = q->last;
  node->next = NULL;
  q->last = node;
  lru_unlock(q);
}

int lru_dismiss(Cache cache) {
  int i;
  LRUQueue q = cache_get_lru_queue(cache);
  lru_lock(q);
  LRUNode node = q->first;
  for (i = 0; i < LRU_FREE_SIZE && node; node = node->next) {
    int suc = cache_try_dismiss(cache, node->idx, node->data_node);
    if (!suc)
      continue;
    lru_remove(q, node);
    lru_free_node(node);
    i++;
  }
  lru_unlock(q);
  log(2, "Desalojo de %d datos", i);
  return i;
}
