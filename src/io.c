#include "io.h"
#include <stdio.h>
#include <stdlib.h>

char* format_stats(struct stats s){ return NULL; }

void usage() {
  fprintf(stderr, "Uso: ./memcache [-n num_threads] [-m memory_size]");
  exit(EXIT_FAILURE);
}
