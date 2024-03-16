#include "io.h"
#include "dalloc.h"
#include "log.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

void usage() {
  const char *name = "./memcached";
  fprintf(stderr, "Usage: %s [-n <num threads>] [-m <memory size>] [-h]\n", name);
  fprintf(stderr, "       %*s [-c <cache cells>] [-r <cache regions>] [-l <log level>]\n",
          (int) strlen(name), "");
  exit(EXIT_FAILURE);
}

