#include <unistd.h>
#include <stdio.h>
#include <syscall.h>
#include <stdlib.h>

int main2(int argc, char** argv) {
  // marg y narg representan si se encontraron argumentos que determinan el limite
  // o numero de hilos
	int opt, marg = 0, narg = 0, tsarg = 0, bsarg = 0;

  int nthreads = sysconf(_SC_NPROCESSORS_ONLN);
  unsigned memsize  = 2UL << 30;

  while ((opt = getopt(argc, argv, "n:m:")) != -1) {
    switch (opt) {
      case 'm':
        memsize = atoi(optarg);
        break;
      case 'n':
        nthreads = atoi(optarg);
        break;
      default:
        printf("Uso del programa\n");
    }
  }

  int text_sock = atoi(argv[optind]);   // Tenemos garantizado que el caller pasa bien los argumentos
  int bin_sock  = atoi(argv[optind + 1]);

  printf("text_sock: %d\n", text_sock);
  printf("bin_sock: %d\n", bin_sock);
  printf("nthreads: %d\n", nthreads);
  printf("memsize: %u\n", memsize);
  return 0;
}

int main() {
  printf("uid: %d\n", getuid());
  if (setuid(0) >= 0)
    printf("Soy root\n");
  printf("uid: %d\n", getuid());
  return 0;
}
