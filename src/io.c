#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

enum IO_STATUS_CODE read_fd(int fd, char buf[], uint64_t size, uint64_t *rc) {
	*rc = read(fd, buf, size);
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return NO_DATA;
	if (rc == 0)
		return CLOSED;
  if (rc < 0)
    return ERROR;
}

void usage() {
  fprintf(stderr, "Uso: ./memcache [-n num_threads] [-m memory_size]");
  exit(EXIT_FAILURE);
}
