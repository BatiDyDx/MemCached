#ifndef __IO_H__
#define __IO_H__

#include <stddef.h>

// Enumeracion de valores usados para indicar ciertos comportamientos comunes
// con funciones de entrada y salida, file descriptors, etc.
enum IO_STATUS_CODE {
  IO_OK,   // No hubo problemas de lectura
  EMPTY,   // No hay informacion para leer
  ERROR,   // Error irrecuperable, cerrar conexion
  CLOSED   // Se cerro la conexion
};

//! @brief Uso del programa. Termina la ejecución de este
void usage(void);

#endif