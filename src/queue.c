#include <stdlib.h>
#include <assert.h>
#include "queue.h"

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
  struct Node *new_node = malloc(sizeof(struct Node));
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
  struct Node *node = q->first;
  if (--q->len == 0)
    q->last = NULL;
  q->first = node->next;
  q->first->prev = NULL;
  free(node);
}

void queue_remove(Queue q, struct Node *node) {
  if (q->first == node)
    q->first = node->next;
  else
    node->prev->next = node->next;
  if (q->last == node)
    q->last = node->prev;
  else
    node->next->prev = node->prev;
  q->len--;
}
