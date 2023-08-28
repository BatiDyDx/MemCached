#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "cache.h"
#include "dalloc.h"
#include "lru.h"
#include "ll.h"

static inline int cmp_keys(char *key1, char *key2, uint64_t len1, uint64_t len2) {
  return len1 == len2 && !memcmp(key1, key2, len1);
}

struct _LLNode {
  Data data;
  LRUNode lru_priority;
  struct _LLNode *prev, *next;
};

Data data_wrap(char *key, uint64_t klen, char *val, uint64_t vlen, char mode) {
  Data data;
  data.key = key;
  data.val = val;
  data.klen = klen;
  data.vlen = vlen;
  data.mode = mode;
  return data;
}

uint32_t list_size() {
  return sizeof(struct _LLNode);
}

List list_init() {
  List list = dalloc(sizeof(struct _LLNode));
  if (list == NULL)
    return NULL;
  list->prev = NULL;
  list->next = NULL;
  return list;
}

void list_free(List list) {
  while (list->next) {
    List node = list->next;
    list->next = node->next;
    list_free_node(node);
  }
  free(list);
}

int list_empty(List list) { return list->next == NULL; }

void list_free_node(List node) {
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
  List new_node = dalloc(sizeof(struct _LLNode));
  if (new_node == NULL)
    return NULL;
  new_node->prev = list;
  if (list->next)
    list->next->prev = new_node;
  new_node->next = list->next;
  list->next = new_node;
  new_node->data = data;
  new_node->lru_priority = NULL;
  return new_node;
}

List list_search(List list, char *key, uint64_t klen) {
  List node;
  for (node = list->next; node; node = node->next) {
    Data data = node->data;
    if (cmp_keys(key, data.key, klen, data.klen))
      return node;
  }
  return NULL;
}

void list_remove(List node) {
  node->prev->next = node->next;
  if (node->next)
    node->next->prev = node->prev;
}

List list_search_and_remove(List list, char *key, uint64_t klen) {
  List node = list_search(list, key, klen);
  if (!node)
    return NULL;
  list_remove(node);
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
