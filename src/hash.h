#ifndef __HASH_H__
#define __HASH_H__

#include "common.h"

typedef struct _HashTable *HashTable;

/**
 * Initializes a hash table with a size given, and the required
 * functions used in the table
 */
HashTable hashtable_init(unsigned size, CopyFunction copy,
                          CompareFunction cmp, DestroyFunction destroy,
                          HashFunction hash);

/**
 * Returns the number of elements in the table
 */
unsigned hashtable_nelems(HashTable table);

/**
 * Returns the size of the table
 */
unsigned hashtable_size(HashTable table);

/**
 * Destroys the table
 */
void hashtable_free(HashTable table);

/**
 * Inserts data in the table, or replaces it if already in the table
 */
void hashtable_insert(HashTable table, void *data);

/**
 * Returns the element that matches with data, or NULL if not in the table
 */
void* hashtable_search(HashTable table, void *data);

/**
 * Removes the element of the table that matches with data
 */
void hashtable_remove(HashTable table, void *data);

#endif /* __HASH_H__ */