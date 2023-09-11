#ifndef __SOCK_H
#define __SOCK_H 1

#include <netinet/in.h>
#define BACKLOG 10

#define EPOLL_LSOCK_FLAGS (EPOLLIN | EPOLLET | EPOLLEXCLUSIVE)
#define EPOLL_CSOCK_FLAGS (EPOLLIN | EPOLLONESHOT)

// Puertos de escucha sin privilegios
static const in_port_t text_port = 8888;
static const in_port_t bin_port  = 8889;

// Puertos de escucha con privilegios
static const in_port_t text_port_priv = 888;
static const in_port_t bin_port_priv  = 889;

//! @brief Crea un socket, lo bindea al puerto port y se pone a escuchar.
//! @param[in] port - puerto de la conexión.
//! @return file descriptor del socket creado.
int mk_tcp_sock(in_port_t port);

//! @brief Realiza los bindings. Utilizará los puertos privilegiados si se
//! tienen los permisos, los cuales suelta antes de terminar. Si no se tienen
//! los privilegios, hace los bindings a los puertos no privilegiados
//! @param[out] text_sock - Almacena el socket de escucha de modo texto
//! @param[out] bin_sock  - Almacena el socket de escucha de modo binario
void make_bindings(int *text_sock, int *bin_sock);

//! @brief Acepta todos los clientes encolados en el socket de escucha segun el
//! modo indicado. Agrega cada conexion nueva a la instancia de epoll del
//! eventloop. Los clientes son agregados en modo EPOLLONESHOT
//! @param eventloop - Contiene el file descriptor de la instancia de epoll
//! y de los sockets de escucha.
//! @param mode - Modo que indica de que socket se aceptan conexiones y en que
//! modo se agrega el cliente
//! @return 0 si no hubo errores, -1 si los hubo al aceptar conexiones
//! IMPORTANTE: Los sockets de escucha debe ser no bloqueantes
int accept_clients(struct eventloop_data eventloop, char mode);

#endif