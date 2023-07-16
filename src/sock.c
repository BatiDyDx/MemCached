#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include "sock.h"
#include "common.h"

int mk_tcp_sock(in_port_t port)
{
	int s, rc;
	struct sockaddr_in sa;
	int yes = 1;

	/*Completar la llamada a socket y descomentar*/
	/*s = socket(..completar ..);
	if (s < 0)
		quit("socket");*/

	rc = setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);
	if (rc != 0)
		quit("setsockopt");

	/*Completar  y descomentar*/
	/* rc = bind(s, ....);
	if (rc < 0)
		quit("bind");
	*/

	rc = listen(s, BACKLOG);
	if (rc < 0)
		quit("listen");

	return s;
}

