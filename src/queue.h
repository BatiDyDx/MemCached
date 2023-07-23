#ifndef __QUEUE_H__
#define __QUEUE_H__

typedef struct _Data {
  char *key, *value;
  unsigned klen, vlen;
  char mode; // TEXT_MODE o BIN_MODE
} Data;

struct Node {
  struct Node *prev, *next;
  Data data;
};

typedef struct _Queue {
  struct Node *first, *last;
  unsigned long len;
} *Queue;

//! @brief Retorna una cola vacia 
Queue queue_init();

//! @brief Libera la memoria de una cola
void queue_free(Queue q);

//! @brief Calcula si una cola es vacia o no
int queue_empty(Queue q);

//! @brief Añade un elemento a la cola
void queue_push(Queue q, Data data);

Data queue_start(Queue q);

//! @brief Desencola el primer elemento
void queue_pop(Queue q);

//! @brief Quita un elemento de la cola. Su uso esta destinado
//! solo para la implementacion interna de la cache. No libera el nodo
void queue_remove(Queue q, struct Node *node);

#endif