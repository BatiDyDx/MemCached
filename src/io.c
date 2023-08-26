#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

enum IO_STATUS_CODE read_fd(struct ClientData *cdata, uint64_t size) {
  long rb = read(cdata->client_fd, cdata->buffer + cdata->current_idx , size);
	if (rb < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return EMPTY;
	if (rb == 0)
		return CLOSED;
  if (rb < 0)
    return ERROR;
  cdata->current_idx += rb;
  return IO_OK;
}

struct ClientData* cdata_init(int csock, int mode) {
  struct ClientData* cdata = dalloc(sizeof(struct ClientData));
	cdata->buf_size = CLIENT_BUF_SIZE;
	cdata->buffer = dalloc(sizeof(cdata->buf_size));
	cdata->client_fd = csock;
	cdata->mode = mode;
	cdata->current_idx = 0;
  return cdata;
} 

void usage() {
  fprintf(stderr, "Uso: ./memcache [-n hilos] [-m memoria] [-c celdas] [-r regiones]");
  exit(EXIT_FAILURE);
}


