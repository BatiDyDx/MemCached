#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <errno.h>
#include "memcached.h"
#include "sock.h"
#include "log.h"
#include "common.h"

int mk_tcp_sock(in_port_t port) {
	int s, rc;
	struct sockaddr_in sa;
	int yes = 1;

	// Socket de conexion por red, con protocolo TCP, lo seteamos como no bloqueante
	s = socket(AF_INET, SOCK_STREAM | SOCK_NONBLOCK, 0);
	if (s < 0)
		quit("Creación de socket de escucha");

	// Configuracion del socket
	rc = setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);
	if (rc != 0)
		quit("setsockopt");

	// Binding del socket al puerto de escucha
	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr.s_addr = INADDR_ANY;
	rc = bind(s, (const struct sockaddr*) &sa, sizeof(sa));
	if (rc < 0)
		quit("Binding de socket a puerto");

	rc = listen(s, BACKLOG);
	if (rc < 0)
		quit("Problemas al escuchar");

	return s;
}

void make_bindings(int *text_sock, int *bin_sock) {
	int text_port, bin_port;
	uid_t uid = getuid(); // consegimos el uid del proceso
	if (uid == 0) {  
		// se ejecuto desde root
		// conexion con privilegios
		text_port = 888;
		bin_port  = 889;
	} else { // conexion sin privilegios
		text_port = 8888;
		bin_port  = 8889;
	}

	log(1, "Conexion de socket modo texto en puerto %d", text_port);
	*text_sock = mk_tcp_sock(text_port);
	if (*text_sock < 0)
		quit("mk_tcp_sock.text");

	log(1, "Conexion de socket modo binario en puerto %d", bin_port);
	*bin_sock = mk_tcp_sock(bin_port);
	if (*bin_sock < 0)
		quit("mk_tcp_sock.bin");

	if (uid == 0) {
		// en root, bajamos privilegios
		char *suid = getenv("SUDO_UID");
		if (suid == NULL)
			quit("getenv");
		if (setuid(atoi(suid)) != 0)
			quit("setuid");
	}
}

// Acepta clientes encolados. Esto es necesario ya que los sockets de escucha
// son tratados por epoll en modo edge-triggered.
// TODO Puede no ser necesario este modo de uso para sockets de escucha, en ese
// caso no seria necesario (aunque tampoco incorrecto) este procedimiento
int accept_clients(struct eventloop_data eventloop, char mode) {
  int csock;
  struct epoll_event event;
  int lsock = mode == TEXT_MODE ? eventloop.text_sock : eventloop.bin_sock;
  while ((csock = accept(lsock, NULL, 0)) >= 0) {
    log(2, "accept fd: %d en modo: %d", csock, mode);
    event.events = EPOLLIN | EPOLLEXCLUSIVE | EPOLLET;
    event.data.u64 = ((uint64_t) mode) << 32 | ((uint64_t) csock);
    epoll_ctl(eventloop.epfd, EPOLL_CTL_ADD, csock, &event);
  }
  if(errno == EAGAIN || errno == EWOULDBLOCK)
    return 0;
  return -1;
}
