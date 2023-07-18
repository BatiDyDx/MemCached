#ifndef __LLIST_H__
#define __LLIST_H__ // Listas enlazadas para datos de tipo texto o binario

#include "common.h"
#include <assert.h>
#include <stdlib.h>

typedef struct _Data {
  char *key, *value;
  unsigned klen, vlen;
  char mode; // TEXT_MODE o BIN_MODE
} Data;

typedef struct _Node {
  Data data;
  struct _Node* next;
} Node;

// Implementación de listas enlazadas para datos de tipo clave-valor en modo texto o binario
typedef Node *List;

//! @return - Lista vacía
List list_init();

//! @brief Libera una lista enlazada
//! @param[in] list - Lista a destruir
void list_free(List list);

//! @brief Determina si una lista es vacia o no
//! @param[in] list
//! @return - 1 si es vacia, 0 si no lo es
int list_empty(List list);

/*
** Adds an element to the start of the list
*/
List list_add(List list, Data *data);

/*
** Removes the first element of the list if its not empty, and returns the
** new first node
*/
List list_remove_start(List list);

/*
** Returns the data from a node that matches with data passed as argument,
** returns NULL if no node matches.
*/
char* list_search(List list, char mode, char* key, unsigned len);

#endif /* __LLIST_H__ */
