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
  char *toks[TEXT_MAX_TOKS];
  uint32_t lens[TEXT_MAX_TOKS];
  enum code op, res;
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
      log(3, "Comando completo de tamaño: %u", req_len);
      op = text_parser(cdata->buffer, toks, lens);
    }
    res = make_cache_request(cache, op, TEXT_MODE, toks, lens, &answer, &ans_len);
    answer_text_client(cdata->fd, res, answer, ans_len);
    if (answer)
      free(answer);

    unsigned rem = cdata->current_idx - req_len;
    memmove(cdata->buffer, cdata->buffer + req_len, rem);
    cdata->current_idx -= req_len;
  }
  return 0;
}

enum code text_parser(char *buf, char *toks[TEXT_MAX_TOKS-1], uint32_t lens[TEXT_MAX_TOKS-1]) {
  enum code op;
  const char *delim = " \t";
  int ntoks = 0;
  char *op_str;
  char* saveptr = NULL;

  op_str = strtok_r(buf, delim, &saveptr);
  if (!op_str)
    return EINVALID;
  ntoks++;
  for (int i = 0; i < TEXT_MAX_TOKS - 1; i++) {
    toks[i] = strtok_r(NULL, delim, &saveptr);
    if (!toks[i])
      break;
    lens[i] = strlen(toks[i]);
    ntoks++;
  }
  op = EUNK;
  if (strtok_r(NULL, buf, &saveptr)) // Hay mas de 3 argumentos
    op = EINVALID;
  else if (!strcmp(op_str, code_str(STATS)))
    op = ntoks == 1 ? STATS : EINVALID;
  else if (!strcmp(op_str, code_str(GET)))
    op = ntoks == 2 ? GET : EINVALID;
  else if (!strcmp(op_str, code_str(DEL)))
    op = ntoks == 2 ? DEL : EINVALID;
  else if (!strcmp(op_str, code_str(PUT)))
    op = ntoks == 3 ? PUT : EINVALID;
  
  if (op == EINVALID)
    log(3, "Comando parseado invalido");
  else if (op == EUNK)
    log(3, "Comando parseado desconocido");
  else
    log(3, "Comando parseado: %s, numero de tokens: %d", code_str(op), ntoks);
	return op;
}

int answer_text_client(int fd, enum code res, char *data, uint64_t len) {
  char c;
  const char *op_string = code_str(res);
  // Bytes totales a escribir
  if (len + strlen(op_string) > TEXT_LIMIT_SIZE) {
    write(fd, "EBIG\n", 5);
    log(3, "Respuesta %s a fd %d", "EBIG\n", fd);
    return 0;
  }
  log(3, "Respuesta %s a fd %d", op_string, fd);
  write(fd, op_string, strlen(op_string));
  if (data) {
    c = ' ';
    write(fd, &c, 1);
    if (write(fd, data, len) < 0){
      return -1;
    }
  }
  c = '\n';
  write(fd, &c, 1);
  return 0;
}
