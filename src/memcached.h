#ifndef __MEMCACHE_H__
#define __MEMCACHE_H__

#define MEM_LIMIT 2UL << 30 // Limite de memcache, en bytes
#define HASH_CELLS 1000000 // Cantidad de celdas en tabla hash

#define TEXT_BUF_SIZE 2048
#define BIN_BUF_SIZE

struct stats {
  unsigned long get, put, del, keys;
};

//! @struct - Estructura para almacenar informacion
struct eventloop_data {
	int epfd; // file descriptor para epoll
	int id;
	int n_proc;
  struct stats text_stats, bin_stats;
};

extern struct eventloop_data system_data;

#endif