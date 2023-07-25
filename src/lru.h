#ifndef __LRU_H__
#define __LRU_H__

#define LRU_FREE_SIZE 10 // Cantidad de elementos a desalojar cuando falta memoria

typedef struct _LRUNode  *LRUNode;
typedef struct _LRUQueue *LRUQueue;

struct _LRUNode {
  unsigned idx;
  List data_node;
  struct _LRUNode *prev, *next;
};

struct _LRUQueue {
  pthread_mutex_t lock;
  struct _LRUNode *first, *last;
};

//! @brief Retorna una cola vacia 
LRUQueue queue_init();

//! @brief Libera la memoria de una cola
void queue_free(LRUQueue q);

//! @brief Calcula si una cola es vacia o no
int queue_empty(LRUQueue q);

//! @brief Añade un elemento a la cola
LRUNode queue_push(LRUQueue q, unsigned idx, List data_node);

//! @brief Quita un elemento de la cola. Su uso esta destinado
//! solo para la implementacion interna de la cache. No libera el nodo
void queue_remove(LRUQueue q, LRUNode node);

int reset_lru_status(LRUQueue q, LRUNode node);

//! @brief Destruye un nodo del tipo LRUNode.
void lru_node_destroy(LRUNode node);

#endif