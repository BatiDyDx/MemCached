#ifndef __IO_H__
#define __IO_H__

#include <stdint.h>
#include "memcached.h"

#define CLIENT_BUF_SIZE 2048
#define MAX_READ_SIZE 1<<12 
struct ClientData {
  char *buffer;
  long buf_size;
  long current_idx;
  int client_fd;
  int mode; 
};

enum IO_STATUS_CODE {
  IO_OK,   // No hubo problemas de lectura
  EMPTY,   // No hay informacion para leer
  ERROR,   // Error irrecuperable, cerrar conexion
  CLOSED   // Se cerro la conexion
};

enum IO_STATUS_CODE read_fd(struct ClientData* buf, uint64_t size);

struct ClientData* cdata_init(int csock, int mode);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif