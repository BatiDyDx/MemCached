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
#include "sock.h"

// Codificamos modo y fd en enteros de 64 bits
#define GET_FD(n) ((int) n)
#define GET_MODE(n) ((int) (n >> 32))

Cache cache;

struct Config {
  unsigned nthreads;
  rlim_t memsize;
  unsigned cache_cells, cache_regions;
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

void handle_client(struct eventloop_data eventloop, int csock, char mode) {
  int status;
  log(2, "handle fd: %d modo: %d", csock, mode);
  if (mode == TEXT_MODE)
    read_fd();
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

void worker_thread(void) {
  int fdc, sock;
  struct epoll_event event;
  while (1) {
    fdc = epoll_wait(eventloop.epfd, &event, 1, -1);
    if (fdc < 0)
      quit("wait en epoll");
    sock = GET_FD(event.data.u64);
    // Aceptar conexiones
    if (sock == eventloop.text_sock)
      accept_clients(eventloop, TEXT_MODE);
    else if (sock == eventloop.bin_sock)
      accept_clients(eventloop, BIN_MODE);
    else // Atender peticion
      handle_client(eventloop, sock, GET_MODE(event.data.u64));
  }
}

void server(int text_sock, int bin_sock, unsigned nthreads) {
  int epfd;
  struct epoll_event event;
  pthread_t threads[nthreads];

  if ((epfd = epoll_create1(0)) < 0)
    quit("Inicializado de epoll");

  eventloop.epfd = epfd;
  eventloop.text_sock = text_sock;
  eventloop.bin_sock = bin_sock;
  
  event.data.fd = text_sock;
  event.events = EPOLLIN | EPOLLEXCLUSIVE | EPOLLET;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, text_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo texto");

  event.data.fd = bin_sock;
  event.events = EPOLLIN | EPOLLEXCLUSIVE | EPOLLET;
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
  config->cache_cells = HASH_CELLS;
  config->cache_regions = HASH_REGIONS;

  while ((opt = getopt(argc, argv, "n:m:r:c:")) != -1) {
    switch (opt) {
      case 'm':
        config->memsize = atoi(optarg);
        break;
      case 'n':
        config->nthreads = atoi(optarg);
        break;
      case 'r':
        config->cache_regions = atoi(optarg);
        break;
      case 'c':
        config->cache_cells = atoi(optarg);
        break;
      default:
        printf("Uso del programa\n");
    }
  }

  return 0;
}

int main(int argc, char **argv) {
  int text_sock, bin_sock;
  struct Config config;

  memcache_config(argc, argv, &config);
  handle_signals();
  make_bindings(&text_sock, &bin_sock);

  /* Función que limita la memoria */
  limit_mem(config.memsize);

  cache = cache_init(config.cache_cells, config.cache_regions);

  /*Iniciar el servidor*/
  server(text_sock, bin_sock, config.nthreads);

  return 0;
}
