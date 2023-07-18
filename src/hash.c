#include "hash.h"
#include "ll.h"
#include <assert.h>
#include <stdlib.h>

/**
 * Tabla hash implementada con encadenamiento externo
 */
struct _HashTable {
  List *elems;
  unsigned num_elems;
  unsigned size;
};

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

void hashtable_insert(HashTable table, char mode, char *key, char *value,
                    unsigned klen, unsigned vlen) {
  Data data = {key, value, klen, vlen, mode};
  unsigned idx = hash(data) % table->size; // TODO determinar funciones de hash
  table->elems[idx] = list_add(table->elems[idx], &data);
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
