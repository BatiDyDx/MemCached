#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

enum IO_STATUS_CODE read_fd(int fd, char buf[], uint64_t size, long *rc) {
	*rc = read(fd, buf, size);
	if (*rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return EMPTY;
	if (*rc == 0)
		return CLOSED;
  if (*rc < 0)
    return ERROR;
  return IO_OK;
}

void usage() {
  fprintf(stderr, "Uso: ./memcache [-n hilos] [-m memoria] [-c celdas] [-r regiones]");
  exit(EXIT_FAILURE);
}
