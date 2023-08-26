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
#include "text_processing.h"
#include "bin_processing.h"
#include "cache.h"
#include "client_data.h"
#include "io.h"
#include "sock.h"

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

void handle_client(struct eventloop_data eventloop, struct ClientData* cdata) {
  int status;
  log(2, "handle fd: %d modo: %d", cdata->client_fd, cdata->mode);
  enum IO_STATUS_CODE err = read_fd(cdata);
  if (err == ERROR || err == CLOSED) // Cerrar
    return;
  else if (cdata->mode == TEXT_MODE)
    status = text_handler(cdata);
  else if (cdata->mode == BIN_MODE)
    status = bin_handler(cdata);
  else
    assert(0);
  if (status < 0) { // Determinar si se cierra la conexion
    close(cdata->client_fd);
    epoll_ctl(eventloop.epfd, EPOLL_CTL_DEL, cdata->client_fd, NULL);
    log(1, "Cierre de conexion con el fd: %d", cdata->client_fd);
    free(cdata->buffer);
    free(cdata);
    return;
  } else if (status == 1) // Mensaje enviado, limpiamos el buffer
    reset_client_info(cdata);
  //struct epoll_event event;
  //POLLET | EPOLLEXCLUSIVE | EPOLLONESHOT;
  //epoll_ctl(eventloop.epfd, EPOLL_CTL_MOD, cdata->client_fd, &events);
}

void worker_thread(void) {
  int fdc, sock;
  struct epoll_event event;
  while (1) {
    fdc = epoll_wait(eventloop.epfd, &event, 1, -1);
    if (fdc < 0)
      quit("wait en epoll");
    struct ClientData *cdata = event.data.ptr;
    sock = cdata->client_fd;
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
  
  event.data.ptr = cdata_init(text_sock, 0); // Se puede ahorrar el buffer
  event.events = EPOLLIN | EPOLLEXCLUSIVE | EPOLLET;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, text_sock, &event) < 0)
    quit("Escucha de epoll en socket de conexion modo texto");

  event.data.ptr = cdata_init(bin_sock, 0);
  event.events = EPOLLIN | EPOLLEXCLUSIVE | EPOLLET;
  if (epoll_ctl(epfd, EPOLL_CTL_ADD, bin_sock, &event) < 0)
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
