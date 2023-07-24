#ifndef __IO_H__
#define __IO_H__

#include <stdint.h>
#include "memcached.h"

enum IO_STATUS_CODE {
  OKEY,      // No hubo problemas de lectura
  NO_DATA, // No hay informacion para leer
  ERROR,   // Error irrecuperable, cerrar conexion
  CLOSED   // Se cerro la conexion
};

enum IO_STATUS_CODE read_fd(int fd, char buf[], uint64_t size, uint64_t *rc);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif