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


int secure_write(int fd, const void* buffer, size_t n){
  ssize_t cant_written;
  size_t total_written;

  const char* buf = buffer;

  for(total_written = 0; total_written < n;){
    cant_written = write(fd, buffer, n);

    if(cant_written < 0 && errno == EINTR) // lectura interrumpida
      continue;
    else
      return -1; // error
    
    buf = buf + cant_written;
    total_written += cant_written;
  }
  return total_written;
}

