#ifndef __MEMCACHE_H__
#define __MEMCACHE_H__

#include <pthread.h>
#include "cache.h"

#define MEM_LIMIT 1UL << 30 // Limite de memcache, en bytes
#define HASH_CELLS 1000000 // Cantidad de celdas en la cache
#define NREGIONS 500 // Cantidad de regiones en que se divide la cache

#define TEXT_BUF_SIZE 2048
#define BIN_BUF_SIZE

extern Cache cache;

//! @struct - Estructura para almacenar informacion
struct eventloop_data {
	int epfd; // file descriptor para epoll
	int id;
  int text_sock, bin_sock;
	int n_proc;
};

int answer_text_client(int fd, enum code res);

#endif