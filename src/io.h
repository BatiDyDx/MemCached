#ifndef __IO_H__
#define __IO_H__

// Enumeracion de valores usados para indicar ciertos comportamientos comunes
// con funciones de entrada y salida, file descriptors, etc.
enum IO_STATUS_CODE {
  IO_OK,   // No hubo problemas de lectura
  EMPTY,   // No hay informacion para leer
  ERROR,   // Error irrecuperable, cerrar conexion
  CLOSED   // Se cerro la conexion
};

//! @brief Vacia el buffer de un file descriptor
//! @return Retorna EMPTY si se vació de forma correcta. La funcion puede
//! devolver alternativamente ERROR o CLOSED
enum IO_STATUS_CODE fd_flush(int fd);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif