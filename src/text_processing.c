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

/* 0: todo ok, continua. -1 errores */
int text_handler(int fd) {
  int blen;
  char buf[TEXT_BUF_SIZE];
  enum IO_STATUS_CODE err;
  while (1) {
		int nread, rem = TEXT_BUF_SIZE - blen;
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
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			op = text_parser(buf,toks,lens);
      switch (op) {
      case GET:
        char *val;
        unsigned vlen;
        // op ahora puede contener el valor de retorno de la operacion en la cache
        //op = cache_get(cache, TEXT_MODE, toks[1], lens[1], &val, &vlen);
        if (op == OK)
          ans();
        else (op == EINVALID)
          ans();
        break;

      case DEL:
        //op = cache_del();
        break;

      case PUT:
        //op = cache_put(cache, TEXT_MODE, toks[1], lens[1], &val, &vlen);
      }
			/*text_handle(evd, p0, len, ....);
				Acá podríamos ver que hacemos con los tokens encontrados:
				toks[0] tendrá PUT, GET, DEL, ó STATS si se ingresó un comando válido.
			*/
			nlen -= len + 1;
			p0 = p;
		}

		/* Si consumimos algo, mover */
		if (p0 != buf) {
			memmove(buf, p0, nlen);
			blen = nlen;
		}
	}
	return 0;
}

enum code text_parser(const char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]) {
	int ntok;
  enum code op;
	log(3, "text parser(%s)", buf);

	/* Separar tokens */
	{
		char *p = buf;
		ntok = 0;
		toks[ntok++] = p;
		while (ntok < TEXT_MAX_TOKS && (p = strchrnul(p, ' ')) && *p) {
			/* Longitud token anterior */
			lens[ntok-1] = p - toks[ntok-1];
			*p++ = 0;
			/* Comienzo nueva token */
			toks[ntok++] = p;
		}
		lens[ntok-1] = p - toks[ntok-1];
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
