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
#include "memcached.h"
#include "sock.h"
#include "common.h"
#include "parser.h"
#include "hash.h"

/* Macro interna */
#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))	\
		return 0;						\
	if (rc <= 0)							\
		return -1;						\
	rc; })

//! @brief Uso del programa memcache. Termina la ejecucion de este
void usage() {
  fprintf(stderr, "Uso: ./memcache [-n num_threads] [-m memory_size]");
  exit(EXIT_FAILURE);
}

/* 0: todo ok, continua. -1 errores */
int text_consume(struct eventloop_data *evd, char buf[TEXT_BUF_SIZE], int fd, int blen) {
  while (1) {
		int rem = TEXT_BUF_SIZE - blen;
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

// Setea el limite de uso de memoria
void limit_mem(rlim_t lim) {
  struct rlimit rl;
  rl.rlim_cur = lim;
  rl.rlim_max = lim;
	setrlimit(RLIMIT_AS, (const struct rlimit*) &rl);
}

void handle_signals() {
/*Capturar y manejar  SIGPIPE */
}

void server(int text_sock, int bin_sock, unsigned nthreads) {
	int epfd;
  struct epoll_event event;
  
  system_data.n_proc = nthreads;
  system_data.id = 0; // TODO Averiguar para que sirve
  if ((epfd = epoll_create1(0)) < 0)
    quit("Inicializado de epoll");
  system_data.epfd = epfd;
  event.events = EPOLLIN;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, text_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo texto");
  if (epoll_ctl(epfd, EPOLL_CTL_ADD,  bin_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo binario");
  
  /* Creacion de threads */
  for (unsigned i = 0; i < nthreads; i++) {
    
  }
}

void memcache_config(int argc, char** argv, unsigned* nthreads, rlim_t* limit) {
  // marg y narg representan si se encontraron argumentos que determinan el limite
  // o numero de hilos
	int opt, marg = 0, narg = 0;
  while ((opt = getopt(argc, argv, "n:m:")) != -1) {
    switch (opt) {
      case 'm':
        *limit = atoi(optarg);
        marg = 1;
        break;
      case 'n':
        *nthreads = atoi(optarg);
        narg = 1;
        break;
      default:
        usage();
    }
  }
  if (!marg)
    *limit = MEM_LIMIT;
  if (!narg)
    *nthreads = sysconf(_SC_NPROCESSORS_ONLN);
}

int main(int argc, char **argv) {
  int text_sock, bin_sock;
  unsigned nthreads;
  rlim_t limit;
	memcache_config(argc, argv, &nthreads, &limit);

	__loglevel = 2;

	handle_signals();

	/*Función que limita la memoria*/
	limit_mem(limit);

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	/*Inicializar la tabla hash, con una dimensión apropiada*/
	hashtable_init(HASH_CELLS, );

	/*Iniciar el servidor*/
	server(text_sock, bin_sock, nthreads);

	return 0;
}
