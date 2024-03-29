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
#include "dalloc.h"
#include "memcached.h"
#include "common.h"
#include "text_protocol.h"
#include "bin_protocol.h"
#include "cache.h"
#include "client_data.h"
#include "io.h"
#include "sock.h"

Cache cache;
struct Config {
  int loglevel;
  unsigned nthreads;
  rlim_t memsize;
  unsigned cache_cells, cache_regions;
};

struct eventloop_data eventloop;

// Setea el limite de uso de memoria
void limit_mem(rlim_t lim) {
  struct rlimit rl;
  rl.rlim_cur = lim * (1 << 20);
  rl.rlim_max = lim * (1 << 20);
  if (setrlimit(RLIMIT_DATA, (const struct rlimit*) &rl) < 0)
    quit("setrlimit");
  log(1, "Seteo limite de memoria a %luMB", lim);
}

void handle_interrupt(int sig) {
  log(1, "Señal %d atrapada", sig);
  close(eventloop.epfd);
  close(eventloop.text_sock);
  close(eventloop.bin_sock);
  exit(EXIT_SUCCESS);
}

void handle_signals() {
  if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
    quit("seteo de signal handler para SIGPIPE");
  if (signal(SIGINT, handle_interrupt) == SIG_ERR)
    quit("seteo de signal handler para SIGINT");
  if (signal(SIGTERM, handle_interrupt) == SIG_ERR)
    quit("seteo de signal handler para SIGTERM");
  log(1, "Configuracion de handlers de señales");
}

void handle_client(struct eventloop_data eventloop, struct ClientData* cdata) {
  int status;
  log(3, "handle fd: %d modo: %d", cdata->fd, cdata->mode);
  enum IO_STATUS_CODE err = client_fill_buffer(cdata);
  if (err == ERROR || err == CLOSED) { // Cerrar
    epoll_ctl(eventloop.epfd, EPOLL_CTL_DEL, cdata->fd, NULL);
    client_close_connection(cdata);
    return;
  }
  if (cdata->mode == TEXT_MODE)
    status = text_handler(cdata);
  else if (cdata->mode == BIN_MODE)
    status = bin_handler(cdata);
  else
    assert(0);
  if (status < 0) { // Determinar si se cierra la conexion
    epoll_ctl(eventloop.epfd, EPOLL_CTL_DEL, cdata->fd, NULL);
    client_close_connection(cdata);
    return;
  }
  struct epoll_event event;
  event.events = EPOLL_CSOCK_FLAGS;
  event.data.ptr = cdata;
  epoll_ctl(eventloop.epfd, EPOLL_CTL_MOD, cdata->fd, &event);
}

void worker_thread(void) {
  int fdc, sock;
  struct epoll_event event;
  while (1) {
    fdc = epoll_wait(eventloop.epfd, &event, 1, -1);
    if (fdc < 0)
      quit("wait en epoll");
    struct ClientData *cdata = event.data.ptr;
    sock = cdata->fd;
    // Aceptar conexiones
    if (sock == eventloop.text_sock)
      accept_clients(eventloop, TEXT_MODE);
    else if (sock == eventloop.bin_sock)
      accept_clients(eventloop, BIN_MODE);
    else // Atender peticion
      handle_client(eventloop, cdata);
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
  
  event.data.ptr = listen_data_init(text_sock);
  event.events = EPOLL_LSOCK_FLAGS;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, text_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo texto");

  event.data.ptr = listen_data_init(bin_sock);
  event.events = EPOLL_LSOCK_FLAGS;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, bin_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo binario");

  log(1, "Configuracion epoll con fd %d", epfd);
  /* Creacion de threads */
  for (unsigned i = 0; i < nthreads; i++)
    pthread_create(threads + i, NULL, (void* (*)(void*)) worker_thread, NULL);
  log(1, "Creacion de %u trabajadores", nthreads);
  pthread_join(threads[0], NULL);
}

int get_config(int argc, char** argv, struct Config *config) {
  int opt;
  config->nthreads = sysconf(_SC_NPROCESSORS_ONLN);
  config->memsize  = MEM_LIMIT;
  config->cache_cells = HASH_CELLS;
  config->cache_regions = HASH_REGIONS;
  config->loglevel  = LOGLEVEL;

  while ((opt = getopt(argc, argv, "n:m:r:c:l:h")) != -1) {
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
      case 'l':
        config->loglevel = atoi(optarg);
        break;
      case 'h':
      default:
        usage();
    }
  }
  return 0;
}

int main(int argc, char **argv) {
  int text_sock, bin_sock;
  struct Config config;

  get_config(argc, argv, &config);
  set_loglevel(config.loglevel);
  handle_signals();
  make_bindings(&text_sock, &bin_sock);

  /* Función que limita la memoria */
  limit_mem(config.memsize);

  cache = cache_init(config.cache_cells, config.cache_regions);

  /*Iniciar el servidor*/
  server(text_sock, bin_sock, config.nthreads);

  return 0;
}
