#ifndef __CLIENT_DATA_H__
#define __CLIENT_DATA_H__

#include "common.h"
#include "io.h"

#define READ_SIZE (1 << 12)
#define BUFFER_SIZE (3 * READ_SIZE)

// La estructura mantiene un buffer para almacenar peticiones hasta poder ser
// procesadas, junto con informacion del cliente
struct ClientData {
  char *buffer;
  uint32_t buf_size;
  uint32_t current_idx;
  int fd;
  int mode;
};

//! @brief Inicializa una estructura dedicada a un socket de escucha. Estos sockets
//! no requieren un buffer ni el resto campos inicializados
struct ClientData* listen_data_init(int lsock);

//! @brief Inicializa una estructura dedicada a un socket de un cliente en un modo
//! dado. Internamente aloja un buffer y el resto de campos.
struct ClientData* client_data_init(int csock, int mode);

//! @brief Rellena el buffer del cliente leyendo del file descriptor asociado.
//! El buffer se completa desde donde se dejó la ultima vez. Lee hasta que no haya
//! mas para leer, el socket se cierre u ocurra algun error.
//! @return IO_OK si no ocurrieron problemas, CLOSED si el socket se cerró y ERROR
//! si sucedió algun error
enum IO_STATUS_CODE client_fill_buffer(struct ClientData *cdata);

//! @brief Incrementa el tamaño del buffer
//! @return -1 si no se pudo alojar memoria, 0 si todo ok
int client_increase_buffer(struct ClientData *cdata);

//! @brief Restaura la estructura a su configuracion inicial (tambien achica el
//! buffer al tamaño inicial). La idea es reiniciar la informacion almacenada
//! de un cliente 
//! @return -1 si no se pudo alojar memoria, 0 si todo ok
int client_reset_info(struct ClientData* cdata);

//! @brief Cierra la conexion con un cliente y libera la estructura
void client_close_connection(struct ClientData *cdata);

#endif