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
#include "bin_protocol.h"

/* 0: todo ok, continua. -1 errores */
int bin_handler(struct ClientData* cdata) {
  char* toks[2];
  uint32_t lens[2];
  int nbytes;
  char *answer;
  uint32_t ans_len;
  enum code op, res;
  while (1) {
    nbytes = bin_parser(cdata->buffer, cdata->current_idx, &op, toks, lens);
    if (nbytes == 0)
      break;
    
    res = make_cache_request(cache, op, BIN_MODE, toks, lens, &answer, &ans_len);
    answer_bin_client(cdata->fd, res, answer, ans_len);
    if (answer)
      free(answer);
    
    unsigned rem = cdata->current_idx - nbytes;
    memmove(cdata->buffer, cdata->buffer + nbytes, rem);
    cdata->current_idx -= nbytes;
  }
  return 1;
}

int bin_parser(char *buf, uint64_t size, enum code *op, char *toks[], uint32_t *lens) {
  int idx;
  int ntoks;
  if (size == 0) // ningun byte
      return 0;
    if (!valid_rq(buf[0])) { // primer byte del buffer
      *op = EUNK;
      return 1;
    }
  *op = buf[0];
  switch (*op) {
    case PUT:
      ntoks = 2;
      break;
    case GET:
    case DEL:
      ntoks = 1;
      break;
    case STATS:
      ntoks = 0;
      break;
    default:
      assert(0);
  }

  idx = 1;
  for (int i = 0; i < ntoks; i++) {
    if (size - idx < 4)
      return 0; // no hay suficientes bytes en el socket para len
    memcpy(lens + i, buf + idx, 4);
    lens[i] = ntohl(lens[i]); // cambiar de big endian a little endian
    idx += 4;
    if(size - idx < lens[i])
      return 0; // no hay suficientes bytes en el socket para la data
    toks[i] = buf + idx;
    idx += lens[i];
  }

  log(2, "Parseo binario, cantidad de bytes: %u", idx);
  return idx;
}

int answer_bin_client(int fd, enum code res, char *data, uint32_t len) {
  log(2, "Respuesta op: %s a %d", code_str(res), fd);
  if (write(fd, &res, 1) < 0)
    return -1;
  if (data) {
    uint32_t len_aux = htonl(len);
    write(fd, &len_aux, 4);
    if (write(fd, data, len) < 0)
      return -1;			
  }
  return 0;
}
