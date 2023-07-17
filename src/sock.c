#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include "sock.h"
#include "common.h"

int mk_tcp_sock(in_port_t port) {
	int s, rc;
	struct sockaddr_in sa;
	int yes = 1;

  // Socket de conexion por red, con protocolo TCP
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		quit("Creación de socket de escucha");

  // Configuracion del socket
	rc = setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);
	if (rc != 0)
		quit("setsockopt");

  // Binding del socket al puerto de escucha
  sa.sin_port = htons(port);
  sa.sin_addr = "";
	rc = bind(s, (const struct sockaddr*) &sa, sizeof(sa));
	if (rc < 0)
		quit("Binding de socket a puerto");

	rc = listen(s, BACKLOG);
	if (rc < 0)
		quit("Problemas al escuchar");

	return s;
}

