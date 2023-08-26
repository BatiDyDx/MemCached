#ifndef __IO_H__
#define __IO_H__

#include <stdint.h>
#include "memcached.h"

#define CLIENT_BUF_SIZE 2048

struct ClientData {
  char *buffer;
  long len;
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

enum IO_STATUS_CODE read_fd(int fd, struct ClientData* buf, uint64_t size);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif