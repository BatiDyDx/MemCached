#include "hash.h"
#include "ll.h"
#include <assert.h>
#include <stdlib.h>

/* EXTERNAL CHAINING HASH TABLES */

/**
 * Main structure representing a hash table
 */
struct _HashTable {
  List *elems;
  unsigned num_elems;
  unsigned size;
  CopyFunction copy;
  CompareFunction cmp;
  DestroyFunction destroy;
  HashFunction hash;
};

HashTable hashtable_init(unsigned size, CopyFunction copy,
                          CompareFunction cmp, DestroyFunction destroy,
                          HashFunction hash) {

  HashTable table = malloc(sizeof(struct _HashTable));
  assert(table != NULL);
  // We ask for 0 initialized memory (every element is a NULL pointer)
  table->elems = calloc(size, sizeof(List));
  assert(table->elems != NULL);
  table->num_elems = 0;
  table->size = size;
  table->copy = copy;
  table->cmp = cmp;
  table->destroy = destroy;
  table->hash = hash;

  for (unsigned idx = 0; idx < size; ++idx)
    assert(table->elems[idx] == NULL);

  return table;
}

unsigned hashtable_nelems(HashTable table) { return table->num_elems; }

unsigned hashtable_size(HashTable table) { return table->size; }

static inline float charge_factor(HashTable table) {
  return ((float) table->num_elems / (float) table->size);
}

void hashtable_free(HashTable table) {
  for (unsigned idx = 0; idx < table->size; ++idx)
    glist_free(table->elems[idx], table->destroy);

  free(table->elems);
  free(table);
  return;
}

void hashtable_insert(HashTable table, void *data) {
  unsigned idx = table->hash(data) % table->size;
  table->elems[idx] = glist_append_start(table->elems[idx], data, table->copy);
}

void* hashtable_search(HashTable table, void *data) {
  unsigned idx = table->hash(data) % table->size;
  return glist_search(table->elems[idx], data, table->cmp);
}

void hashtable_remove(HashTable table, void *data) {
  unsigned idx = table->hash(data) % table->size;
  List list = table->elems[idx];
  // If list is empty, return
  if (glist_empty(list))
    return;
  // If the element to remove is in the first node of the list then we update
  // the table to point to the second node and free the first one
  else if (table->cmp(data, list->data) == 0) {
    table->num_elems--;
    table->elems[idx] = glist_remove_start(list, table->destroy);
    return;
  } else {
    while (!glist_empty(list->next)) {
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
