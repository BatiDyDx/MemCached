#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
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
    list = list_remove_start(list);
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

/* Tabla hash implementada con encadenamiento externo */
struct _HashTable {
  List *elems;
  unsigned num_elems;
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

HashTable hashtable_init(unsigned size) {
  HashTable table = malloc(sizeof(struct _HashTable));
  assert(table != NULL);
  // We ask for 0 initialized memory (every element is a NULL pointer)
  table->elems = calloc(size, sizeof(List));
  assert(table->elems != NULL);
  table->num_elems = 0;
  table->size = size;
  return table;
}

unsigned hashtable_size(HashTable table) { return table->size; }

void hashtable_free(HashTable table) {
  for (unsigned idx = 0; idx < table->size; ++idx)
    list_free(table->elems[idx]);
  free(table->elems);
  free(table);
}

void hashtable_insert(HashTable table, struct Node *node) {
  // TODO - Desalojar si no hay espacio
  unsigned idx = hash_bytes(node->data.key, node->data.klen) % table->size;
  table->elems[idx] = list_add(table->elems[idx], node);
}

void* hashtable_search(HashTable table, void *data) {
  unsigned idx = table->hash(data) % table->size;
  return list_search(table->elems[idx], data, table->cmp);
}

void hashtable_remove(HashTable table, void *data) {
  unsigned idx = table->hash(data) % table->size;
  List list = table->elems[idx];
  // If list is empty, return
  if (list_empty(list))
    return;
  // If the element to remove is in the first node of the list then we update
  // the table to point to the second node and free the first one
  else if (table->cmp(data, list->data) == 0) {
    table->num_elems--;
    table->elems[idx] = list_remove_start(list, table->destroy);
    return;
  } else {
    while (!list_empty(list->next)) {
      if (table->cmp(data, list->next->data) == 0) {
        List tmp = list->next;
        table->num_elems--;
        list->next = tmp->next;
        table->destroy(tmp->data);
        free(tmp);
        break;
      }
    }
  }
}
