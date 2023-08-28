#ifndef __MEMCACHE_H__
#define __MEMCACHE_H__

#include <pthread.h>
#include "cache.h"

#define MEM_LIMIT 1UL << 30 // Limite de memcache, en bytes
#define HASH_CELLS 1000000 // Cantidad de celdas en la cache
#define HASH_REGIONS 500 // Cantidad de regiones en que se divide la cache

#define TEXT_LIMIT_SIZE 2048
#define BIN_BUF_SIZE

extern Cache cache;

//! @struct - Estructura para almacenar informacion
struct eventloop_data {
	int epfd; // file descriptor para epoll
  int text_sock, bin_sock;
};


#endif