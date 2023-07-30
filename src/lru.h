#ifndef __LRU_H__
#define __LRU_H__

#include "ll.h"
#include "cache.h"

typedef struct _Cache *Cache;

#define LRU_FREE_SIZE 10 // Cantidad de elementos a desalojar cuando falta memoria

typedef struct _LRUNode  *LRUNode;
typedef struct _LRUQueue *LRUQueue;

void lru_free_node(LRUNode node);

//! @brief Retorna una cola vacia 
LRUQueue lru_init();

//! @brief Libera la memoria de una cola
void lru_destroy(LRUQueue q);

//! @brief Calcula si una cola es vacia o no
int lru_empty(LRUQueue q);

//! @brief Añade un elemento a la cola
LRUNode lru_push(LRUQueue q, unsigned idx, List data_node);

//! @brief Quita un elemento de la cola. Su uso esta destinado
//! solo para la implementacion interna de la cache. No libera el nodo
void lru_remove(LRUQueue q, LRUNode node);

void reset_lru_status(LRUQueue q, LRUNode node);

int lru_dismiss(Cache cache);

#endif