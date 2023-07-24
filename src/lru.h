#ifndef __LRU_H__
#define __LRU_H__

#define LRU_FREE_SIZE 10 // Cantidad de elementos a desalojar cuando falta memoria

typedef struct _LRUNode  *LRUNode;
typedef struct _LRUQueue *LRUQueue;

//! @brief Retorna una cola vacia 
LRUQueue queue_init();

//! @brief Libera la memoria de una cola
void queue_free(LRUQueue q);

//! @brief Calcula si una cola es vacia o no
int queue_empty(LRUQueue q);

//! @brief Añade un elemento a la cola
void queue_push(LRUQueue q, unsigned idx, List data_node);

//! @brief Quita un elemento de la cola. Su uso esta destinado
//! solo para la implementacion interna de la cache. No libera el nodo
void queue_remove(LRUQueue q, LRUNode node);

int reset_lru_status(LRUQueue q, LRUNode node);

#endif