#ifndef __LINKED_LIST_H__
#define __LINKED_LIST_H__

#include "lru.h"
#include "cache.h"

// Listas doblemente enlazadas con sentinela
typedef struct _LLNode *List;

List list_init();

uint32_t list_size();

void list_free(List list);

int list_empty(List list);

List list_free_node(List node);

Data list_get_data(List list);

void list_set_data(List list, Data data);

LRUNode list_get_lru_priority(List list);

void list_set_lru_priority(List list, LRUNode lru_priority);

void list_insert(List list, Data data);

List list_search(List list, char mode, char *key, uint64_t klen);

List list_remove(List list, char mode, char *key, uint64_t klen);

List list_remove_first(List list);

#endif