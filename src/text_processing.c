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
#include "client_data.h"
#include "log.h"
#include "dalloc.h"
#include "stats.h"

static enum code make_cache_request(enum code op, char *toks[3], int lens[3], char **answer, unsigned *ans_len);

/* 0: todo ok, continua. -1 errores */
int text_handler(struct ClientData *cdata) {
  char *ebyte; // end byte
  char *toks[3];
  int lens[3];
  enum code op, res;
  int fd = cdata->fd;
  unsigned req_len; // Longitud de la peticion a procesar
  char *answer;
  unsigned ans_len;

  while ((ebyte = memchr(cdata->buffer, '\n', cdata->current_idx))) {  
    if (!ebyte) // Mensaje incompleto
      return 0;
    req_len = ebyte - cdata->buffer;
    if (req_len >= TEXT_LIMIT_SIZE) { // Mensaje muy largo
      log(3, "Comando invalido: es muy largo");
      op = EINVALID;
    } else {
      *ebyte = '\0';
      //log(1, "Comando completo: <%s>", cdata->buffer);
      op = text_parser(cdata->buffer, toks, lens);
    }
    
    res = make_cache_request(op, toks, lens, &answer, &ans_len);
    answer_text_client(fd, res, answer, ans_len);
    if (answer)
      free(answer);
    
    unsigned rem = cdata->current_idx - (req_len + 1);
    memmove(cdata->buffer, cdata->buffer + (req_len + 1), rem);
    cdata->current_idx -= req_len + 1;
  }
  return 1;
}

enum code make_cache_request(enum code op, char *toks[3], int lens[3],
                             char **answer, unsigned *ans_len) {
  const size_t stats_size = 2000;
  enum code res;
  switch (op) {
      case PUT:
        char *key, *value;
        key = dalloc(sizeof(char) * lens[1]);
        value = dalloc(sizeof(char) * lens[2]);
        if (!key || !value) {
          *answer = NULL;
          *ans_len = 0;
          return EOOM;
        }
        memcpy(key, toks[1], lens[1]);
        memcpy(value, toks[2], lens[2]);
        res = cache_put(cache, TEXT_MODE, key, lens[1], value, lens[2]);
        *answer = NULL;
        *ans_len = 0;
        break;

      case DEL:
        res = cache_del(cache, TEXT_MODE, toks[1], lens[1]);
        *answer = NULL;
        *ans_len = 0;
        break;

      case GET:
        res = cache_get(cache, TEXT_MODE, toks[1], lens[1], answer, ans_len);
        break;

      case STATS:
        struct Stats stats_buf = stats_init();
        res = cache_stats(cache, TEXT_MODE, &stats_buf);
        if (res == OK) {
          *answer = dalloc(stats_size);
          if (*answer == NULL)
            return EOOM;
          *ans_len = format_stats(&stats_buf, *answer, stats_size);
        } else {
          *ans_len = 0;
          *answer = NULL;
        }
        break;

      case EINVALID:
        res = EINVALID;
        *answer = NULL;
        *ans_len = 0;
        break;

      default:
        assert(0);
        break;
    }

    return res;
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
    log(3, "Comando parseado: %s, numero de tokens: %d", code_str(op), ntoks);
  else
    log(3, "Comando parseado invalido");
	return op;
}

int answer_text_client(int fd, enum code res, char *data, uint64_t len) {
  char c;
  const char *op_string = code_str(res);
  // Bytes totales a escribir
  if (len + strlen(op_string) > TEXT_LIMIT_SIZE) {
    write(fd, "EBIG\n", 5);
    log(2, "Respuesta %s a fd %d", "EBIG\n", fd);
    return 0;
  }
  log(2, "Respuesta %s a fd %d", op_string, fd);
  write(fd, op_string, strlen(op_string));
  if (data) {
    c = ' ';
    write(fd, &c, 1);
    write(fd, data, len);
  }
  c = '\n';
  write(fd, &c, 1);
  return 0;
}
