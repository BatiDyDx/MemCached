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
#include "text_processing.h"
#include "hash.h"
#include "io.h"

struct eventloop_data eventloop;

// Setea el limite de uso de memoria
void limit_mem(rlim_t lim) {
  struct rlimit rl;
  rl.rlim_cur = lim;
  rl.rlim_max = lim;
	setrlimit(RLIMIT_DATA, (const struct rlimit*) &rl);
}

void handle_interrupt(int _sig) {
  close(eventloop.epfd);
  close(eventloop.text_sock);
  close(eventloop.bin_sock);
  //hashtable_free(cache);
  exit(EXIT_SUCCESS);
}

void handle_signals() {
  struct sigaction s;
  s.sa_handler = SIG_IGN;
  if (!sigaction(SIGPIPE, (const struct sigaction*) &s, NULL));
    quit("seteo de sigaction para SIGPIPE");

  s.sa_handler = handle_interrupt;
  if (!sigaction(SIGINT, (const struct sigaction*) &s, NULL));
    quit("seteo de sigaction para SIGINT");
  if (!sigaction(SIGTERM, (const struct sigaction*) &s, NULL));
    quit("seteo de sigaction para SIGTERM");
}

//void enable_conn_privilege() {
//  capset();
//}

void* worker_thread(void* _arg) {
  int fdc;
  struct epoll_event event;
  while (1) {
    int csock, mode;
    fdc = epoll_wait(eventloop.epfd, &event, 1, -1);
    if (fdc < 0)
      quit("wait en epoll");
    // Aceptar conexiones
    if (event.data.fd == eventloop.text_sock || event.data.fd == eventloop.bin_sock) {
      mode = event.data.fd == eventloop.text_sock ? TEXT_MODE : BIN_MODE;
      csock = accept(eventloop.text_sock, NULL, 0);
      log(2, "accept fd: %d modo: %d", csock, mode);
      if (csock < 0)
        quit("accept de nueva conexion");
      event.data.u64 = ((uint64_t) mode) << 32 | ((uint64_t) csock);
      epoll_ctl(eventloop.epfd, EPOLL_CTL_ADD, csock, &event);
    } else { // Atender peticion
      int status;
      csock = event.data.u64 & 0xFFFFFFFF;
      mode  = (event.data.u64 & 0xFFFFFFFF00000000) >> 32;
      log(2, "handle fd: %d modo: %d", csock, mode);
      if (mode == TEXT_MODE)
        status = text_handler(csock);
      else if (mode == BIN_MODE)
        status = bin_handler(csock);
      else
        assert(0);
    }
  }
}

void server(int text_sock, int bin_sock, unsigned nthreads) {
	int epfd;
  struct epoll_event event;
  pthread_t dummy_thread; // TODO Quizas hace falta almacenar info sobre hilos
  
  eventloop.n_proc = nthreads;
  eventloop.id = 0; // TODO Averiguar para que sirve
  if ((epfd = epoll_create1(0)) < 0)
    quit("Inicializado de epoll");
  eventloop.epfd = epfd;
  event.events = EPOLLIN | EPOLLEXCLUSIVE;

  eventloop.text_sock = text_sock;
  event.data.fd = text_sock;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, text_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo texto");

  eventloop.bin_sock = bin_sock;
  event.data.fd = bin_sock;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD,  bin_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo binario");

  /* Creacion de threads */
  for (unsigned i = 0; i < nthreads; i++)
    pthread_create(&dummy_thread, NULL, worker_thread, NULL);
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
	__loglevel = 2;

	memcache_config(argc, argv, &nthreads, &limit);
	handle_signals();
  enable_conn_privilege();

	/*Función que limita la memoria*/
	limit_mem(limit);

	text_sock = mk_tcp_sock(mc_lport_text);
	if (text_sock < 0)
		quit("mk_tcp_sock.text");

	bin_sock = mk_tcp_sock(mc_lport_bin);
	if (bin_sock < 0)
		quit("mk_tcp_sock.bin");

	/*Inicializar la tabla hash, con una dimensión apropiada*/
	hashtable_init(HASH_CELLS);

	/*Iniciar el servidor*/
	server(text_sock, bin_sock, nthreads);

	return 0;
}
