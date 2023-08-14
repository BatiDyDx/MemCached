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

/* 0: todo ok, continua. -1 errores */
int text_handler(int fd) {
  int blen = 0;
  char buf[TEXT_BUF_SIZE];
  enum IO_STATUS_CODE err;
  while (1) {
    long nread, rem = TEXT_BUF_SIZE - blen;
    assert(rem >= 0);
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0)
			return -1;
    
    err = read_fd(fd, buf + blen, rem, &nread);
    if (err == NO_DATA)
      return 0;
    else if (err == ERROR || err == CLOSED)
      return -1;

		log(3, "Read %i bytes from fd %i", nread, fd);
		blen += nread;
    char *p, *p0 = buf;
		int nlen = blen;

    /* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
      /* Mensaje completo */
      enum code op;
      enum code res;
      char *val;
      unsigned vlen;
			int len = p - p0;
			*p++ = 0;
			log(3, "full command: <%s>", p0);
			char *toks[3];
			int lens[3] = {0};
			op = text_parser(buf,toks,lens);
      switch (op) {
      case PUT:
        log(3, "text parse: PUT %s, len %u, %s, len %u", toks[1], lens[1], toks[2], lens[2]);
        char *key, *value;
        key = dalloc(sizeof(char) * lens[1]);
        value = dalloc(sizeof(char) * lens[2]);
        memcpy(key, toks[1], lens[1]);
        memcpy(value, toks[2], lens[2]);
        res = cache_put(cache, TEXT_MODE, key, lens[1], value, lens[2]);
        answer_text_client(fd, res, NULL, 0);
        break;

      case DEL:
        log(3, "text parse: DEL %s, len %u", toks[1], lens[1]);
        res = cache_del(cache, TEXT_MODE, toks[1], lens[1]);
        answer_text_client(fd, res, NULL, 0);
        break;

      case GET:
        log(3, "text parse: GET %s, len %u", toks[1], lens[1]);
        res = cache_get(cache, TEXT_MODE, toks[1], lens[1], &val, &vlen);
        answer_text_client(fd, res, val, vlen);
        break;

      case STATS:
        log(3, "text parse: STATS");
        struct Stats stats_buf = stats_init();
        char buf[1000]; // Tamaño arbitrario, podria ser mas chico
        int len;
        enum code res = cache_stats(cache, TEXT_MODE, &stats_buf);
        if (res == OK)
          len = format_stats(&stats_buf, buf, 1000);
        answer_text_client(fd, res, buf, len);
        break;

      case EINVALID:
        log(3, "text parse: invalid instruction");
        answer_text_client(fd, EINVALID, NULL, 0);
        break;

      default:
        assert(0);
        break;
      }
			nlen -= len + 1;
			p0 = p;
		}

		/* Si consumimos algo, mover */
		if (p0 != buf) {
			memmove(buf, p0, nlen);
			blen = nlen;
		}
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

  if (strtok(NULL, buf)) // Mas de 3 argumentos es un comando invalido
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
	log(3, "op code: %d, ntoks = %i", op, ntoks);
	return op;
}

int answer_text_client(int fd, enum code res, char *data, uint64_t len) {
  log(2, "Respuesta op: %d a %d", res, fd);
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
