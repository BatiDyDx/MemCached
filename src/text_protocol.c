#define _GNU_SOURCE /* strchrnul */
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "common.h"
#include "memcached.h"
#include "text_protocol.h"
#include "io.h"
#include "cache.h"
#include "client_data.h"
#include "log.h"
#include "dalloc.h"
#include "stats.h"

/* 0: todo ok, continua. -1 errores */
int text_handler(struct ClientData *cdata) {
  char *ebyte; // end byte
  char *toks[TEXT_MAX_TOKS - 1];
  uint32_t lens[TEXT_MAX_TOKS - 1];
  enum code op, res;
  int fd = cdata->fd;
  unsigned req_len; // Longitud de la peticion a procesar
  char *answer;
  uint32_t ans_len;

  while ((ebyte = memchr(cdata->buffer, '\n', cdata->current_idx))) {
    req_len = (ebyte - cdata->buffer) + 1;
    if (req_len > TEXT_LIMIT_SIZE) { // Mensaje muy largo
      log(3, "Comando invalido: es muy largo");
      op = EINVALID;
    } else {
      *ebyte = '\0';
      log(3, "Comando completo: <%s>", cdata->buffer);
      op = text_parser(cdata->buffer, toks, lens);
    }
    
    res = make_cache_request(cache, op, TEXT_MODE, toks, lens, &answer, &ans_len);
    answer_text_client(fd, res, answer, ans_len);
    if (answer)
      free(answer);
    
    unsigned rem = cdata->current_idx - req_len;
    memmove(cdata->buffer, cdata->buffer + req_len, rem);
    cdata->current_idx -= req_len;
  }
  return 1;
}

enum code text_parser(char *buf, char *toks[TEXT_MAX_TOKS-1], uint32_t lens[TEXT_MAX_TOKS-1]) {
  enum code op;
  const char *delim = " \t\n";
  int ntoks = 0;
  char *op_str;

  op_str = strtok(buf, delim);
  if (op_str)
    ntoks++;
  for (int i = 0; i < TEXT_MAX_TOKS - 1; i++) {
    toks[i] = strtok(NULL, delim);
    if (!toks[i])
      break;
    lens[i] = strlen(toks[i]);
    ntoks++;
  }

  if (strtok(NULL, buf)) // Hay mas de 3 argumentos
    op = EINVALID;
  else if (ntoks == 1 && !strcmp(op_str, code_str(STATS)))
    op = STATS;
  else if (ntoks == 2 && !strcmp(op_str, code_str(GET)))
    op = GET;
  else if (ntoks == 2 && !strcmp(op_str, code_str(DEL)))
    op = DEL;
  else if (ntoks == 3 && !strcmp(op_str, code_str(PUT)))
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
