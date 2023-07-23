#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <pthread.h>
#include "queue.h"
#include "hash.h"

/* Listas enlazadas simples - Dedicadas a uso interno de la tabla hash */

struct _LLNode {
  struct Node *node; // Nodo de la cola, no confundir con el siguiente de la lista
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

List list_add(List list, struct Node *node) {
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
void hashtable_insert(HashTable table, struct Node *node) {
  // TODO - Posible optimizacion: La interfaz de cache busca el nodo y si no esta
  // lo inserta, esto lleva a caclular dos veces el hash
  unsigned idx = hash_bytes(node->data.key, node->data.klen) % table->size;
  pthread_mutex_t *lock = table->region_locks + (idx % table->nregions);
  pthread_mutex_lock(lock);
  table->elems[idx] = list_add(table->elems[idx], node);
  pthread_mutex_unlock(lock);
}

struct Node* hashtable_search(HashTable table, char* key, unsigned klen) {
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
