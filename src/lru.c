#define _GNU_SOURCE
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <pthread.h>
#include "cache.h"
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

static inline int lock_queue(LRUQueue q) {
  return pthread_mutex_lock(&q->lock);
}

static inline int unlock_queue(LRUQueue q) {
  return pthread_mutex_unlock(&q->lock);
}

LRUQueue queue_init() {
  LRUQueue q = malloc(sizeof(struct _LRUQueue));
  assert(q);
  q->first = q->last = NULL;
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&q->lock, &attr);
  pthread_mutexattr_destroy(&attr);
  return q;
}

void queue_free(LRUQueue q) {
  LRUNode node = q->first;
  while (node) {
    LRUNode next = node->next;
    free(node);
    node = next;
  }
  free(q);
  pthread_mutex_destroy(&q->lock);
}

int queue_empty(LRUQueue q) {
  int b;
  lock_queue(q);
  b = !q->first;
  unlock_queue(q);
  return b;
}

LRUNode queue_push(LRUQueue q, unsigned idx, List data_node) {
  LRUNode new_node = malloc(sizeof(struct _LRUNode));
  assert(new_node);
  new_node->idx = idx;
  new_node->data_node = data_node;
  
  lock_queue(q);
  q->last = new_node;
  if (queue_empty(q))
    q->first = new_node;
  else
    q->last->next = new_node;

  new_node->prev = q->last;
  new_node->next = NULL;
  unlock_queue(q);
  return new_node;
}

void queue_remove(LRUQueue q, LRUNode node) {
  lock_queue(q);
  if (q->first == node)
    q->first = node->next;
  else
    node->prev->next = node->next;
  if (q->last == node)
    q->last = node->prev;
  else
    node->next->prev = node->prev;
  unlock_queue(q);
}

void reset_lru_status(LRUQueue q, LRUNode node) {
  lock_queue(q);
  queue_remove(q, node);

  if (queue_empty(q))
    q->first = node;
  else
    q->last->next = node;
  node->prev = q->last;
  node->next = NULL;
  q->last = node;
  unlock_queue(q);
}

int lru_dismiss(Cache cache) {
  int i;
  LRUQueue q = cache_get_lru_queue(cache);
  lock_queue(q);
  LRUNode node = q->first;
  for (i = 0; i < LRU_FREE_SIZE && node; node = node->next) {
    int suc = cache_try_dismiss(cache, node->idx, node->data_node);
    if (!suc)
      continue;
    queue_remove(q, node); // Ojo, queue_remove bloquea la cola. Para solucionar esto podemos usar mutex recursivos
    lru_free_node(node);
    i++;
  }
  unlock_queue(q);
  return i;
}
