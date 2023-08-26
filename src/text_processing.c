#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "common.h"
#include "memcached.h"
#include "text_processing.h"
#include "io.h"
#include "cache.h"
#include "log.h"
#include "dalloc.h"
#include "stats.h"

static inline min(int x, int y) {
  return x <= y ? x : y;
}

/* 0: todo ok, continua. -1 errores */
int text_handler(struct ClientData *cdata) {
  long nread;
  char buf[TEXT_LIMIT_SIZE];
  enum IO_STATUS_CODE err;
  char *ebyte; // end byte
  char *toks[3];
  int lens[3];
  enum code op, res;
  int fd = cdata->client_fd;
  nread = min(cdata->current_idx, 2048);
  ebyte = memchr(cdata->buffer, '\n', nread);

  if (!ebyte && nread == 2048) { // Mensaje muy largo
    log(3, "Comando invalido: es muy largo");
    op = EINVALID;
  } else if (!ebyte) { // Mensaje incompleto 
    return 0;
  } else {
    *ebyte = '\0';
    log(3, "Comando completo: <%s>", buf);
    op = text_parser(buf,toks,lens);
  }

  switch (op) {
    case PUT:
      char *key, *value;
      key = dalloc(sizeof(char) * lens[1]);
      if (!key)
        return -1;
      value = dalloc(sizeof(char) * lens[2]);
      if (!value)
        return -1;
      memcpy(key, toks[1], lens[1]);
      memcpy(value, toks[2], lens[2]);
      res = cache_put(cache, TEXT_MODE, key, lens[1], value, lens[2]);
      answer_text_client(fd, res, NULL, 0);
      break;

    case DEL:
      res = cache_del(cache, TEXT_MODE, toks[1], lens[1]);
      answer_text_client(fd, res, NULL, 0);
      break;

    case GET:
      char *val;
      unsigned vlen;
      res = cache_get(cache, TEXT_MODE, toks[1], lens[1], &val, &vlen);
      answer_text_client(fd, res, val, vlen);
      break;

    case STATS:
      struct Stats stats_buf = stats_init();
      char buf[1000]; // Tamaño arbitrario, podria ser mas chico
      int len;
      enum code res = cache_stats(cache, TEXT_MODE, &stats_buf);
      if (res == OK)
        len = format_stats(&stats_buf, buf, 1000);
      answer_text_client(fd, res, buf, len);
      break;

    case EINVALID:
      answer_text_client(fd, EINVALID, NULL, 0);
      break;

    default:
      assert(0);
      break;
  }
  return 0;
}

enum code text_parser(char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]) {
  enum code op;
  const char *delim = " \t\n";
  int ntoks = 0;

  toks[0] = strtok(buf, delim);
  if (toks[0])
    ntoks++;
  for (int i = 1; i < TEXT_MAX_TOKS; i++) {
    toks[i] = strtok(NULL, delim);
    if (!toks[i])
      break;
    lens[i] = strlen(toks[i]);
    ntoks++;
  }

  // Hay mas de 3 argumentos
  if (strtok(NULL, buf))
    op = EINVALID;
  else if (ntoks == 1 && !strcmp(toks[0], code_str(STATS)))
    op = STATS;
  else if (ntoks == 2 && !strcmp(toks[0], code_str(GET)))
    op = GET;
  else if (ntoks == 2 && !strcmp(toks[0], code_str(DEL)))
    op = DEL;
  else if (ntoks == 3 && !strcmp(toks[0], code_str(PUT)))
    op = PUT;
  else
    op = EINVALID;

  if (op != EINVALID)
    log(3, "Comando parseado: %d, numero de tokens: %d", op, ntoks);
  else
    log(3, "Comando parseado invalido");
	return op;
}

int answer_text_client(int fd, enum code res, char *data, uint64_t len) {
  log(2, "Respuesta %d a fd %d", res, fd);
  char c; 
  const char *op_string = code_str(res);
  if (write(fd, op_string, strlen(op_string)) < 0)
    return -1;
  if (data) {
    c = ' ';
    write(fd, &c, 1);
    if (write(fd, data, len) < 0)
      return -1;
  }
  c = '\n';
  write(fd, &c, 1);
  return 0;
}
