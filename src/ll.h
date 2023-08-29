#ifndef __LINKED_LIST_H__
#define __LINKED_LIST_H__

#include <stdint.h>

// Esta estructura representa una llave-valor a almacenar en la cache
typedef struct {
  char *key, *val;
  uint64_t klen, vlen;
  char mode;
} Data;

typedef struct _LRUNode *LRUNode;

// Listas doblemente enlazadas con sentinela
typedef struct _LLNode *List;

//! @brief Envuelve varios valores en una estructura Data
Data data_wrap(char *key, uint64_t klen, char *value, uint64_t vlen, char mode);

//! @brief Inicializa una lista enlazada
List list_init();

//! @brief Retorna el tamaño de un nodo en bytes
uint32_t list_size();

//! @brief Libera una lista enlazada
void list_free(List list);

//! @brief Determina si una lista es vacia o no
int list_empty(List list);

//! @brief Libera un nodo y los valores almacenados en el
void list_free_node(List node);

//! @brief Retorna la informacion almacenada en el nodo
Data list_get_data(List list);

//! @brief Actualiza la informacion almacenada en el nodo
void list_set_data(List list, Data data);

//! @brief Retorna la referencia al nodo en la cola LRU asociado con el nodo argumento
LRUNode list_get_lru_priority(List list);

//! @brief Actualiza la prioridad de un nodo en la lista seteando un nuevo nodo en la cola LRU
void list_set_lru_priority(List list, LRUNode lru_priority);

//! @brief Inserta a una lista enlazada con la informacion pasada
List list_insert(List list, Data data);

//! @brief Busca en una lista enlazada, suministrando la llave y longitud
List list_search(List list, char *key, uint64_t klen);

//! @brief Remueve un nodo de la lista
//! IMPORTANTE: No libera el nodo, solo lo quita de la lista
void list_remove(List node);

//! @brief Busca un nodo, si lo encuentra lo quita
//! @return Retorna el nodo en caso de haber sido hallado
List list_search_and_remove(List list, char *key, uint64_t klen);

#endif