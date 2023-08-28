#include "io.h"
#include "dalloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

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

void usage(char *name) {
  fprintf(stderr, "Uso: %s [-n hilos] [-m memoria] [-c celdas] [-r regiones]", name);
  exit(EXIT_FAILURE);
}


