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

//! @brief Funcion de escritura segura propuesta por el libro: 
//! "The linux programming interface - Kerrisk"
//! mencionado en la pagina 1255.
//! @param[in] fd File descriptor desde el cual se realizará la lectura.
//! @param[out] buffer Buffer que almacenará los valores leidos.
//! @param[in] n Cantidad de bytes a leer desde el file descriptor.
//! @return Cantidad de bytes leidos. En caso de error, se retorna -1.
int secure_write(int fd, const void* buffer, size_t n);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif