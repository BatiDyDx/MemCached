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
        res = cache_put(cache, TEXT_MODE, toks[1], lens[1], toks[2], lens[2]);
			  // manejar errores de res (EINVALID, etc);
        // se malloquea memoria para el nombre del comando toks[0] hay que liberar!
        // hay que agregar destruccion de toks;
        answer_text_client(fd, res);
        break;

      case DEL:
        log(3, "text parse: DEL %s, len %u", toks[1], lens[1]);
        res = cache_del(cache, TEXT_MODE, toks[1], lens[1]);
        answer_text_client(fd, res);
        break;

      case GET:
        log(3, "text parse: GET %s, len %u", toks[1], lens[1]);
        res = cache_get(cache, TEXT_MODE, toks[1], lens[1], &val, &vlen);
        answer_text_client(fd, res);
        break;

      case STATS:
        log(3, "text parse: STATS");
        struct Stats stats_buf = stats_init();
        enum code res = cache_stats(cache, TEXT_MODE, &stats_buf);
        answer_text_client(fd, res);
        break;

      case EINVALID:
        log(3, "text parse: invalid instruction");
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

// enum code text_parser(char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]) {
// 	int ntok;
//   enum code op;
// 	log(3, "text parser(%s)", buf);
// 	/* Separar tokens */
// 	{
// 		char *p = buf, *aux_buf;
// 		ntok = 0;
// 		aux_buf = p;
// 		while (ntok < TEXT_MAX_TOKS - 1 && (p = strchrnul(p, ' ')) && *p) {
// 			/* Longitud token anterior */
//       lens[ntok] = p - aux_buf;
//       toks[ntok] = malloc(lens[ntok]);
// 			memmove(toks[ntok],aux_buf,lens[ntok]);
//       ntok++;
//       *p++ = 0; // ¿Se puede remover?
// 			/* Comienzo nueva token */
// 			aux_buf = p;
// 		}
//     lens[ntok] = p - aux_buf;
// 		toks[ntok] = malloc(lens[ntok]);
//     memmove(toks[ntok], aux_buf, lens[ntok]);
//     ntok++;
// 	}
  
//   printf("op %s\n",toks[0]);
//   printf("ntok %i\n",ntok);
//   if (ntok == 1 && !strcmp(toks[0], code_str(STATS)))
//     op = STATS;
//   else if (ntok == 2 && !strcmp(toks[0], code_str(GET)))
//     op = GET;
//   else if (ntok == 2 && !strcmp(toks[0], code_str(DEL)))
//     op = DEL;
//   else if (ntok == 3 && !strcmp(toks[0], code_str(PUT)))
//     op = PUT;
//   else
//     op = EINVALID;
// 	log(3, "op code: %d, ntok = %i", op, ntok);
//   if (EINVALID)
//     return EINVALID;
//   for ()
// 	return op;
// }

enum code text_parser(char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]) {
  enum code op;
  const char *delim = " \t\n";
  int ntoks = 0;

  toks[0] = strtok(buf, delim);
  if (toks[0])
    ntoks++;
  for (int i = 1; i < TEXT_MAX_TOKS; i++) {
    toks[i] = strtok(NULL, delim);
    if (toks[i])
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
  if (op != EINVALID && op != STATS) {
    for (int i = 1; i < ntoks; i++) {
      lens[i] = strlen(toks[i]);
      char* tmp = malloc(lens[i]);
      memcpy(tmp, toks[i], lens[i]);
      toks[i] = tmp;
    }
  }
	return op;
}
