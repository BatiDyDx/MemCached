#ifndef __BIN_PROC_H__
#define __BIN_PROC_H__

#define BIN_MAX_TOKS 3

#include "client_data.h"

//! @brief Función de manejo de entrada para clientes en modo binario.
//! @param cdata - Informacion del cliente por atender
//! @return 1 si el pedido fue atendido correctamente, 0 si el mensaje no esta
//! completo, -1 si no hay memoria disponible
int bin_handler(struct ClientData* cdata);

//! @brief Parser del modo binario para poder evaluar comandos en el protocolo binario
//! @param cdata - Info de cliente del cual se parsea el comando
//! @param toks  - Parametro de salida para almacenar llave y valor (cuando los haya). Estos valores
//! son alocados por el parser, el array toks debe tener espacio suficiente
//! @param lens  - Parametro de salida donde se guardan las longitudes en bytes de la llave
//! y valor (cuando los haya). El array debe tener tamaño suficiente
//! @param ntoks - Cantidad de tokens a parsear. Determinados segun la operacion
int bin_parser(char *buf, uint64_t size, enum code *op, char *toks[], uint32_t *lens);

//! @brief Rutina para responder a clientes en modo binario segun el protocolo
//! @return 0 si no hay errores, -1 si los hay
int answer_bin_client(int fd, enum code res, char *data, uint32_t len);

#endif