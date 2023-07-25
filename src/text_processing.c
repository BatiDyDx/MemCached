#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "common.h"
#include "text_processing.h"
#include "io.h"
#include "cache.h"
#include "log.h"
#include "stats.h"

extern Cache cache;

/* 0: todo ok, continua. -1 errores */
int text_handler(int fd) {
  int blen;
  char buf[TEXT_BUF_SIZE];
  enum IO_STATUS_CODE err;
  while (1) {
    uint64_t nread, rem = TEXT_BUF_SIZE - blen;
    assert (rem >= 0);
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
			int len = p - p0;
			*p++ = 0;
			log(3, "full command: <%s>", p0);
			char *toks[3];
			int lens[3] = {0};
			int ntok;
			op = text_parser(buf,toks,lens);
      enum code res;
      switch (op) {
      case PUT:
        log(3, "binary parse: PUT %s %s", toks[1], toks[2]);
        res = cache_put(cache, TEXT_MODE,  toks[1], lens[1], toks[2], lens[2]);
			  // manejar errores de res (EINVALID, etc);
        // se malloquea memoria para el nombre del comando toks[0] hay que liberar!
        // hay que agregar destruccion de toks;
        break;

      case DEL:
        log(3, "text parse: DEL %s", toks[1]);
        res = cache_del(cache, TEXT_MODE, toks[1], lens[1]);
			  break;

      case GET:
			  char* val;
		  	unsigned vlen;
        log(3, "text parse: GET %s", toks[1]);
	  		res = cache_get(cache, TEXT_MODE, toks[1], lens[1], &val, &vlen);
        break;

      case STATS:
      	log(3, "text parse: STATS");
        struct Stats stats_buf = stats_init();
			  enum code res = cache_stats(cache, TEXT_MODE, &stats_buf);
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

enum code text_parser(unsigned char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]) {
	int ntok;
  enum code op;
	log(3, "text parser(%s)", buf);
	/* Separar tokens */
	{
		char*p = buf;
    char* aux_buf;
		ntok = 0;
		aux_buf = p;
		while (ntok < TEXT_MAX_TOKS - 1 && (p = strchrnul(p, ' ')) && *p) {
			/* Longitud token anterior */
      lens[ntok] = p - aux_buf;
      toks[ntok] = malloc(lens[ntok]);
			memmove(toks[ntok++],aux_buf,lens[ntok]);
      *p++ = 0;
			/* Comienzo nueva token */
			aux_buf = p;
		}
		toks[ntok] = malloc(lens[ntok]);
    strcpy(toks[ntok++], aux_buf);

	}
  if (ntok == 1 && !strcmp(toks[0], code_str(STATS)))
    op = STATS;
  else if (ntok == 2 && !strcmp(toks[0], code_str(GET)))
    op = GET;
  else if (ntok == 2 && !strcmp(toks[0], code_str(DEL)))
    op = DEL;
  else if (ntok == 3 && !strcmp(toks[0], code_str(PUT)))
    op = PUT;
  else
    op = EINVALID;
	log(3, "op code: %d, ntok = %i", op, ntok);
	return op;
}
