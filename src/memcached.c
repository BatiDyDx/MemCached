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
#include "common.h"
#include "text_processing.h"
#include "bin_processing.h"
#include "cache.h"
#include "io.h"

Cache cache;

struct Config {
  unsigned nthreads;
  rlim_t memsize;
  int text_sock, bin_sock;
};

struct eventloop_data eventloop;

// Setea el limite de uso de memoria
void limit_mem(rlim_t lim) {
  struct rlimit rl;
  rl.rlim_cur = lim;
  rl.rlim_max = lim;
  setrlimit(RLIMIT_DATA, (const struct rlimit*) &rl);
  log(3, "Seteo limite de memoria a %luMB", lim / (1 << 20));
}

void handle_interrupt(int sig) {
  log(2, "Señal %d atrapada", sig);
  close(eventloop.epfd);
  close(eventloop.
  text_sock);
  close(eventloop.bin_sock);
  cache_destroy(cache);
  exit(EXIT_SUCCESS);
}

void handle_signals() {
  struct sigaction s;
  s.sa_handler = SIG_IGN;
  if (sigaction(SIGPIPE, (const struct sigaction*) &s, NULL) < 0)
    quit("seteo de sigaction para SIGPIPE");

  s.sa_handler = handle_interrupt;
  if (sigaction(SIGINT, (const struct sigaction*) &s, NULL) < 0)
    quit("seteo de sigaction para SIGINT");
  if (sigaction(SIGTERM, (const struct sigaction*) &s, NULL) < 0)
    quit("seteo de sigaction para SIGTERM");
  log(3, "Configuracion de handlers de señales");
}

void worker_thread(void) {
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
      mode  = (event.data.u64  >> 32) & 0xFFFFFFFF;
      log(2, "handle fd: %d modo: %d", csock, mode);
      if (mode == TEXT_MODE)
        status = text_handler(csock);
      else if (mode == BIN_MODE)
        status = bin_handler(csock);
      else
        assert(0);
      if (status < 0) { // Determinar si se cierra la conexion
        close(csock);
        epoll_ctl(eventloop.epfd, EPOLL_CTL_DEL, csock, NULL);
        log(1, "Cierre de conexion con el fd: %d", csock);
      }
    }
  }
}

void server(int text_sock, int bin_sock, unsigned nthreads) {
  int epfd;
  struct epoll_event event;
  pthread_t threads[nthreads]; // TODO Quizas hace falta almacenar info sobre hilos

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

  log(2, "Configuracion epoll con fd %d", epfd);
  /* Creacion de threads */
  for (unsigned i = 0; i < nthreads; i++)
    pthread_create(threads + i, NULL, (void* (*)(void*)) worker_thread, NULL);
  log(3, "Creacion de %u trabajadores", nthreads);
  pthread_join(threads[0], NULL);
}

int memcache_config(int argc, char** argv, struct Config *config) {
  // marg y narg representan si se encontraron argumentos que determinan el limite
  // o numero de hilos
  int opt;
  config->nthreads = sysconf(_SC_NPROCESSORS_ONLN);
  config->memsize  = MEM_LIMIT;

  while ((opt = getopt(argc, argv, "n:m:")) != -1) {
    switch (opt) {
      case 'm':
        config->memsize = atoi(optarg);
        break;
      case 'n':
        config->nthreads = atoi(optarg);
        break;
      default:
        printf("Uso del programa\n");
    }
  }

  config->text_sock = atoi(argv[optind]);   // Tenemos garantizado que el caller pasa bien los argumentos
  config->bin_sock  = atoi(argv[optind + 1]);

  return 0;
}

int main(int argc, char **argv) {
  struct Config config;

  memcache_config(argc, argv, &config);
  handle_signals();

  /* Función que limita la memoria */
  limit_mem(config.memsize);

  cache = cache_init(HASH_CELLS, NREGIONS);

  /*Iniciar el servidor*/
  server(config.text_sock, config.bin_sock, config.nthreads);

  return 0;
}
