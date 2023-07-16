#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include "sock.h"
#include "common.h"

#include "parser.h"

/* Macro interna */
#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))	\
		return 0;						\
	if (rc <= 0)							\
		return -1;						\
	rc; })

/* 0: todo ok, continua. -1 errores */
int text_consume(struct eventloop_data *evd, char buf[2024], int fd, int blen)
{
	while (1) {
		int rem = sizeof *buf - blen;
		assert (rem >= 0);
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0)
			return -1;
		int nread = READ(fd, buf + blen, rem);

		log(3, "Read %i bytes from fd %i", nread, fd);
		blen += nread;
		char *p, *p0 = buf;
		int nlen = blen;

		/* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Mensaje completo */
			int len = p - p0;
			*p++ = 0;
			log(3, "full command: <%s>", p0);
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(buf,toks,lens);

			/*text_handle(evd, p0, len, ....);
				Acá podríamos ver que hacemos con los tokens encontrados:
				toks[0] tendrá PUT, GET, DEL, ó STATS si se ingresó un comando válido.
			*/
			nlen -= len + 1;
			p0 = p;
		}

		/* Si consumimos algo, mover */
		if (p0 != buf) {
			memmove(buf, p0, nlen);
			blen = nlen;
		}
	}
	return 0;
}

void limit_mem()
{
	/*Implementar*/
}

void handle_signals()
{
/*Capturar y manejar  SIGPIPE */
}

void server(int text_sock, int bin_sock)
{

	/*Configurar Epoll
	+
	Creación de threads necesarios*/
	/*La cantidad de threads debe ser fija al iniciar el servidor
	todos los thread tendran acceso a la misma estructura epoll 
	e iran manejando los eventos que vayan apareciendo.
	*/

	/*En algún momento al manejar eventos de tipo EPOLLIN de un cliente 
	en modo texto invocaremos a text_consume: 
	int rc;
	rc = text_consume(evd, buff, fd, blen);
	y  al parecido habrá que hacer al momento al manejar eventos de tipo 
	EPOLLIN de un cliente en modo binario.	
	*/
}

int main(int argc, char **argv)
{
	
	int text_sock, bin_sock;

	__loglevel = 2;

	handle_signals();

	/*Función que limita la memoria*/
	limit_mem();

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");


	/*Inicializar la tabla hash, con una dimensión apropiada*/
	/* 1 millón de entradas, por ejemplo*/
	/* .....*/


	/*Iniciar el servidor*/
	server(text_sock, bin_sock);

	return 0;
}
