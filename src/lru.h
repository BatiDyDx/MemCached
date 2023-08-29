#ifndef __LRU_H__
#define __LRU_H__

#include "ll.h"
#include "cache.h"

typedef struct _Cache *Cache;

#define LRU_FREE_SIZE 10 // Cantidad de elementos a desalojar cuando falta memoria

typedef struct _LRUNode  *LRUNode;
typedef struct _LRUQueue *LRUQueue; // Cola que implementa prioridades LRU

//! @brief Inicializa una cola LRU 
LRUQueue lru_init();

//! @brief Libera un nodo de la cola
void lru_free_node(LRUNode node);

//! @brief Libera una cola
void lru_destroy(LRUQueue q);

//! @brief Retorna si una cola es vacia o no
int lru_empty(LRUQueue q);

//! @brief Añade un elemento al final de la cola con la informacion dada
//! @return Retorna el nodo creado
LRUNode lru_push(LRUQueue q, unsigned idx, List data_node);

//! @brief Quita un elemento de la cola. Su uso esta destinado
//! solo para la implementacion interna de la cache. No libera el nodo
void lru_remove(LRUQueue q, LRUNode node);

//! @brief Reinicia la prioridad de un nodo. Esto es, lo quita de su posición
//! actual y lo inserta al final de la cola
void reset_lru_status(LRUQueue q, LRUNode node);

//! @brief Desaloja valores de la cache seleccionados segun la prioridad LRU
//! @return Retorna la cantidad de elementos desalojados
int lru_dismiss(Cache cache);

#endif