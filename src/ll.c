#include <stdlib.h>
#include <assert.h>
#include "cache.h"
#include "lru.h"
#include "ll.h"

//#define CMP_KEYS(K1, LEN1, K2, LEN2) (LEN1 == LEN2 && memcmp(K1, K2, LEN1))
static inline int cmp_keys(char mode1, char mode2, char *key1, char *key2, uint64_t len1, uint64_t len2) {
  return mode1 == mode2 && len1 == len2 && !memcmp(key1, key2, len1);
}

struct _LLNode {
  Data data;
  LRUNode lru_priority;
  struct _LLNode *prev, *next;
};

uint32_t list_size() {
  return sizeof(struct _LLNode);
}

List list_init() {
  List list = malloc(sizeof(struct _LLNode));
  assert(list);
  list->prev = NULL;
  list->next = NULL;
  return list;
}

void list_free(List list) {
  while (list) {
    List next = list->next;
    list_free_node(list);
  }
}

int list_empty(List list) { return list->next == NULL; }

List list_free_node(List node) {
  free(node->data.key);
  free(node->data.val);
  free(node);
}

Data list_get_data(List list) {
  return list->data;
}

void list_set_data(List list, Data data) {
  list->data = data;
}

LRUNode list_get_lru_priority(List list) {
  return list->lru_priority;
}

void list_set_lru_priority(List list, LRUNode lru_priority) {
  list->lru_priority = lru_priority;
}

List list_insert(List list, Data data) {
  List new_node = malloc(sizeof(struct _LLNode));
  assert(new_node != NULL);
  new_node->prev = list;
  new_node->next = list->next;
  new_node->data = data;
  new_node->lru_priority = NULL;
  return new_node;
}

List list_search(List list, char mode, char *key, uint64_t klen) {
  List node;
  for (node = list->next; node; node = node->next) {
    Data data = node->data;
    if (cmp_keys(mode, data.mode, key, data.key, klen, data.klen))
      return node;
  }
  return NULL;
}

void list_remove(List node) {
  node->prev->next = node->next;
  if (node->next)
    node->next->prev = node->prev;
}

List list_search_and_remove(List list, char mode, char *key, uint64_t klen) {
  List node = list_search(list, mode, key, klen);
  if (!node)
    return NULL;
  list_remove(list);
  return node;
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

void ll_node_destroy(List node) {
  lru_node_destroy(node->lru_priority);
  free(node);
}
