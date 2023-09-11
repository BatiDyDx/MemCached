#include "io.h"
#include "dalloc.h"
#include "log.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

void usage(char *name) {
  fprintf(stderr, "Uso: %s [-n hilos] [-m memoria] [-c celdas] [-r regiones]", name);
  exit(EXIT_FAILURE);
}

