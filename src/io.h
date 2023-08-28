#ifndef __IO_H__
#define __IO_H__

enum IO_STATUS_CODE {
  IO_OK,   // No hubo problemas de lectura
  EMPTY,   // No hay informacion para leer
  ERROR,   // Error irrecuperable, cerrar conexion
  CLOSED   // Se cerro la conexion
};

enum IO_STATUS_CODE fd_flush(int fd);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif