#include "io.h"
#include "dalloc.h"
#include "log.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

void usage(char *name) {
  fprintf(stderr, "Uso: %s [-n hilos] [-m memoria]\n", name);
  fprintf(stderr, "     %*s [-c celdas] [-r regiones] [-l loglevel]\n", (int) strlen(name), "");
  exit(EXIT_FAILURE);
}

