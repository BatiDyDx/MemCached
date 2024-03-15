#ifndef __MEMCACHE_H__
#define __MEMCACHE_H__

#include <pthread.h>
#include "cache.h"

#define MEM_LIMIT 1024     // Limite de memcache, en megabytes
#define HASH_CELLS 1000000 // Cantidad de celdas en la cache
#define HASH_REGIONS 500   // Cantidad de regiones en que se divide la cache
#define LOGLEVEL 3         // Nivel de logging por defecto

extern Cache cache;

struct eventloop_data {
	int epfd; // file descriptor para epoll
  int text_sock, bin_sock; // Sockets de escucha para modo texto y binario
};

#endif
