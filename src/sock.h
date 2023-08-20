#ifndef __SOCK_H
#define __SOCK_H 1

#include <netinet/in.h>
#define BACKLOG 10

static const in_port_t text_port = 8888;
static const in_port_t bin_port  = 8889;

static const in_port_t text_port_priv = 888;
static const in_port_t bin_port_priv  = 889;

//! @brief Crea un socket, lo bindea al puerto port y se pone a escuchar .
//!
//! @param[in] port - puerto de la conexión.
//! @param[out] socket - file descriptor del socket creado.
int mk_tcp_sock(in_port_t port);

//! @brief Realiza los bindings 
//! @param[out] text_sock - Almacena el socket de escucha de modo texto
//! @param[out] bin_sock  - Almacena el socket de escucha de modo binario
void make_bindings(int *text_sock, int *bin_sock);

int accept_clients(struct eventloop_data eventloop, char mode);

#endif
