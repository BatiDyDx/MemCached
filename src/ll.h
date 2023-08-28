#ifndef __LINKED_LIST_H__
#define __LINKED_LIST_H__

#include <stdint.h>

typedef struct {
  char *key, *val;
  uint64_t klen, vlen;
  char mode;
} Data;

typedef struct _LRUNode *LRUNode;

// Listas doblemente enlazadas con sentinela
typedef struct _LLNode *List;

Data data_wrap(char *key, uint64_t klen, char *value, uint64_t vlen, char mode);

List list_init();

uint32_t list_size();

void list_free(List list);

int list_empty(List list);

void list_free_node(List node);

Data list_get_data(List list);

void list_set_data(List list, Data data);

LRUNode list_get_lru_priority(List list);

void list_set_lru_priority(List list, LRUNode lru_priority);

List list_insert(List list, Data data);

List list_search(List list, char *key, uint64_t klen);

void list_remove(List node);

List list_search_and_remove(List list, char *key, uint64_t klen);

List list_remove_first(List list);

void ll_node_destroy(List node);

#endif