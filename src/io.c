#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

enum IO_STATUS_CODE read_fd(int fd, struct ClientData *buf, uint64_t size) {
  buf->len = read(fd, buf->buffer, size);
	if (buf->len < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return EMPTY;
	if (buf->len == 0)
		return CLOSED;
  if (buf->len < 0)
    return ERROR;
  buf->current_idx = buf->len;
  return IO_OK;
}

void usage() {
  fprintf(stderr, "Uso: ./memcache [-n hilos] [-m memoria] [-c celdas] [-r regiones]");
  exit(EXIT_FAILURE);
}


