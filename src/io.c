#include "io.h"
#include "dalloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

enum IO_STATUS_CODE read_fd(struct ClientData *cdata) {
  long rb;
  int stop = 0;
  while (!stop) {
    if (cdata->current_idx + MAX_READ_SIZE > cdata->buf_size) {
      log(3, "Realloc de buffer para fd %d", cdata->client_fd);
      cdata->buffer = drealloc(cdata->buffer, cdata->buf_size, 3 * MAX_READ_SIZE);
      if (cdata->buffer == NULL)
        return ERROR;
      cdata->buf_size += MAX_READ_SIZE;
    }
    rb = read(cdata->client_fd, cdata->buffer + cdata->current_idx, MAX_READ_SIZE);
    log(3, "Leidos %d bytes de fd %d", rb, cdata->client_fd);
    if (rb < MAX_READ_SIZE)
      stop = 1;
    if (rb > 0)
      cdata->current_idx += rb;
  }
	if (rb < 0 && (errno != EAGAIN && errno != EWOULDBLOCK))
		return ERROR;
	if (rb == 0)
		return CLOSED;
  return IO_OK;
}

enum IO_STATUS_CODE fd_flush(int fd) {
  char buf[1024];
  int rc;
  while ((rc = read(fd, buf, 1024)) >= 0);
  log(3, "Flush al fd %d", fd);
  if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return EMPTY;
	if (rc == 0)
		return CLOSED;
  return ERROR;
}

void usage() {
  fprintf(stderr, "Uso: ./memcache [-n hilos] [-m memoria] [-c celdas] [-r regiones]");
  exit(EXIT_FAILURE);
}


