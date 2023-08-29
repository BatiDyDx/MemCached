#include <assert.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "cache.h"
#include "common.h"
#include "io.h"
#include "log.h"
#include "dalloc.h"
#include "stats.h"
#include "memcached.h"
#include "bin_processing.h"

/* 0: todo ok, continua. -1 errores */
int bin_handler(struct ClientData* cdata) {
  if(cdata->current_idx == 0) // ningun byte
    return 0; // Estas 5 lineas podemos meterlas dentro del parser
  char op = cdata->buffer[0]; // primer byte del buffer
  if (!valid_rq(op)) 
    op = EINVALID;
  
  char* toks[2];
  uint32_t lens[2];
  int ret;
  enum code res;

  switch (op){
  case PUT:
    ret = bin_parser(cdata, toks, lens, 2); // consumiremos 2 argumentos
    if(ret <= 0) // En caso de haber EOOM deberiamos mandarselo al cliente
      return ret;
    res = cache_put(cache, BIN_MODE,  toks[0], lens[0], toks[1], lens[1]);
    answer_bin_client(cdata, res, NULL, 0);
    break;
  
  case DEL: 
    ret = bin_parser(cdata, toks, lens, 1); // cosumiremos 1 argumento
    if(ret <= 0)
      return ret;
    res = cache_del(cache, BIN_MODE, toks[0], lens[0]); 
    answer_bin_client(cdata, res, NULL, 0);
    break;
  
  case GET:
    ret = bin_parser(cdata, toks, lens, 1); // consumiremos 1 argumento
    if(ret <= 0)
      return ret;
    char* val;
    unsigned vlen;
    res = cache_get(cache, BIN_MODE, toks[0], lens[0], &val, &vlen);
    answer_bin_client(cdata, res, val, vlen);
    break;

  case STATS:
    char buf[1000];
    struct Stats stats_buf;
    int len;
    res = cache_stats(cache, BIN_MODE, &stats_buf);
    if (res == OK)
      len = format_stats(&stats_buf, buf, 1000);
    answer_bin_client(cdata, res, buf, len);
    break;

  case EUNK:
  case EINVALID:
    answer_bin_client(cdata, op, NULL, 0);
    break;

  default:
    assert(0);
    return -1;  
  }
  fd_flush(cdata->fd);
  return 1;
}

int bin_parser(struct ClientData *cdata, char *toks[], uint32_t *lens , int ntoks) {
  int idx = 1;
  for (int i = 0; i < ntoks; i++) {
      if(cdata->current_idx - idx < 4)
        return 0; // no hay suficientes bytes en el socket para len
      memcpy(lens + i, (cdata->buffer + idx),4);
      lens[i] = ntohl(lens[i]); // cambiar de big endian a little endian
      idx += 4;
      if(cdata->current_idx - idx < lens[i])
        return 0; // no hay suficientes byes en el socket para la data
      idx += lens[i];
  }
  idx = 1;
  for (int i = 0; i < ntoks; i++){
    if (!(toks[i] = dalloc(lens[i])))
      return -1;
    idx += 4;
    memcpy(toks[i],(cdata->buffer + idx), lens[i]);// se carga el argumento 
    idx += lens[i];
  }

  return 1;
}

int answer_bin_client(struct ClientData* cdata, enum code res, char *data, uint32_t len) {
  log(2, "Respuesta op: %d a %d", res, cdata->fd);
  if (write(cdata->fd, &res, 1) < 0)
    return -1;
  if (data) {
    uint32_t len_aux = htonl(len);
    write(cdata->fd, &len_aux, 4);
    if (write(cdata->fd, data, len) < 0)
      return -1;			
  }
  return 0;
}
