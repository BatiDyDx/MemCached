#ifndef __HASH_H__
#define __HASH_H__

#include "common.h"

// Tabla hash para datos de forma clave-valor en modo texto o binario
typedef struct _HashTable *HashTable;

//! @brief Inicializa una tabla hash para ser usada como cache
//! @param size Cantidad de celdas de la cache
//! @return Cache inicializada
HashTable hashtable_init(unsigned size);

unsigned hashtable_nelems(HashTable table);

//! @brief Libera la memoria ocupada por una cache y sus datos almacenados
void hashtable_free(HashTable table);

void hashtable_insert(HashTable table, char mode, char *key, char *value,
                    unsigned klen, unsigned vlen);

/**
 * Returns the element that matches with data, or NULL if not in the table
 */
void* hashtable_search(HashTable table, void *data);

/**
 * Removes the element of the table that matches with data
 */
void hashtable_remove(HashTable table, void *data);

#endif /* __HASH_H__ */