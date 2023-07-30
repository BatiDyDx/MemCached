#ifndef __CACHE_H__
#define __CACHE_H__

#include "common.h"
#include "lru.h"
#include "cache.h"
#include "ll.h"
#include "stats.h"

typedef struct _Cache *Cache;

//! @brief Inicializa un sistema de cache
//! La misma trabaja con una politica de desalojo LRU (Least Recently Used)
//! @param size Tamaño de la cache
Cache cache_init(uint64_t size, uint64_t nregions);

void cache_destroy(Cache cache);

//! @brief Recupera un dato almacenado en una cache
//! @param[in] cache Cache en cuestión sobre la cual se realiza la operacion
//! @param[in] mode Modo en que la llave y valor estan almacenados. Puede ser TEXT_MODE o BIN_MODE
//! @param[in] key LLave asociada al valor buscado
//! @param[in] klen Tamaño en bytes de la llave.
//! @param[out] vlen Si se encuentra el valor, se almacena aqui su tamaño
//! @return Puntero al valor buscado, NULL si no esta en la cache
enum code cache_get(Cache cache, char mode, char* key, unsigned klen, char **val, unsigned *vlen);

//! @brief Inserta un valor a la cache asociandolo con una llave. Si el valor ya
//! se encontraba, lo sobreescribe
//! @param[in] cache Cache en cuestión sobre la cual se realiza la operacion
//! @param[in] mode Modo en que la llave y valor estan almacenados. Puede ser TEXT_MODE o BIN_MODE
//! @param[in] key LLave asociada al valor a guardar
//! @param[in] klen Tamaño en bytes de la llave.
//! @param[in] value Valor a ser almacenado en la cache
//! @param[in] vlen Tamaño en bytes del valor
enum code cache_put(Cache cache, char mode, char* key, unsigned klen, char *value, unsigned vlen);

//! @brief Elimina el valor de la cache asociado a la llave. No realiza cambios
//! si la llave no esta presente
//! @param[in] cache Cache en cuestión sobre la cual se realiza la operacion
//! @param[in] mode Modo en que la llave y valor estan almacenados. Puede ser TEXT_MODE o BIN_MODE
//! @param[in] key LLave asociada al valor a guardar
//! @param[in] klen Tamaño en bytes de la llave.
enum code cache_del(Cache cache, char mode, char* key, unsigned klen);

//! @brief Retorna las estadisticas de uso de una cache. Las estadisticas se
//! almacenan segun el tipo del dato, texto o binario.
//! @param[in] cache Cache sobre la cual se piden las estadisticas de uso
//! @param[in] mode Modo sobre el cual se piden las estadisticas. Puede ser TEXT_MODE o BIN_MODE
enum code cache_stats(Cache cache, char mode, struct Stats* stats);

LRUQueue cache_get_lru_queue(Cache cache);

int cache_try_dismiss(Cache cache, uint64_t idx, List data_node);

#endif