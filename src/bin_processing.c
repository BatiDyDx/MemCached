#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "cache.h"
#include "common.h"
#include "io.h"
#include "log.h"
#include "dalloc.h"
#include "stats.h"
#include "memcached.h"
#include "bin_processing.h"


/* 0: todo ok, continua. -1 errores */
int bin_handler(int fd) {
	long nread;
	char op;  
	enum IO_STATUS_CODE err;

	while(1) {
		err = read_fd(fd, &op, sizeof(char), &nread);
		if (err == NO_DATA)
			return 0; // No hay comandos
		else if (err == ERROR || err == CLOSED)
      return -1;

    if (!valid_rq(op)) return EINVALID; // Comando invalido

		char* toks[2];
		int lens[2];
		int ntoks;
		enum code res;
    (void) res;
    (void) ntoks;
		switch (op){
		case PUT:
			ntoks = bin_parser(fd, toks, lens, 2); // consumiremos 2 argumentos
			log(3, "binary parse: PUT %s %s", toks[0], toks[1]);
			res = cache_put(cache, BIN_MODE,  toks[0], lens[0], toks[1], lens[1]);
      		answer_bin_client(fd, res, NULL, 0);
			break;
		
		case DEL: 
			ntoks = bin_parser(fd, toks, lens, 1); // cosumiremos 1 argumento
			log(3, "binary parse: DEL %s %d", toks[0], lens[0]);
			res = cache_del(cache, BIN_MODE, toks[0], lens[0]); 
      		answer_bin_client(fd, res, NULL, 0);
			break;
		
		case GET:
			ntoks = bin_parser(fd, toks, lens, 1); // consumiremos 1 argumento
			log(3, "binary parse: GET %s %d", toks[0], toks[1]);
			char* val;
			unsigned vlen;
			res = cache_get(cache, BIN_MODE, toks[0], lens[0], &val, &vlen);
      		answer_bin_client(fd, res, val, vlen);
			break;

		case STATS:
			log(3, "binary parse: STATS");
			char buf[1000];
			struct Stats stats_buf;
			int len;
			res = cache_stats(cache, BIN_MODE, &stats_buf);
			if (res == OK)
          		len = format_stats(&stats_buf, buf, 1000);
        	answer_bin_client(fd, res, buf, len);
			break;
    	}
		break;
	}
	return 0;
}

int bin_parser(int fd, char *toks[], int *lens , int ntoks) {
	for (int i = 0; i < ntoks; i++) {
		read(fd, lens + i, 4); // se lee la longitud del argumento
    	lens[i] = ntohl(lens[i]);
	  	if (!(toks[i] = dalloc(lens[i])))
			return -1;
		read(fd, toks[i], lens[i]);// se lee el argumento 
	}
	return ntoks;
}

int answer_bin_client(int fd, enum code res, char *data, uint32_t len) {
  log(2, "Respuesta op: %d a %d", res, fd);
  if (write(fd, &res, 1) < 0)
    return -1;
  if (data) {
	uint32_t len_aux = htonl(len);
    write(fd, &len_aux, 4);
    if (write(fd, data, len) < 0)
      return -1;			
  }
  return 0;
}