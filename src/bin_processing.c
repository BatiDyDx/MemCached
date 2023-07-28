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
    (void) ntoks;
		switch (op){
		case PUT:
			ntoks = bin_parser(fd, toks, lens, 2); // consumiremos 2 argumentos
			log(3, "binary parse: PUT %s %s", toks[0], toks[1]);
			res = cache_put(cache, BIN_MODE,  toks[0], lens[0], toks[1], lens[1]);
      answer_client(fd, res);
			break;
		
		case DEL: 
			ntoks = bin_parser(fd, toks, lens, 1); // cosumiremos 1 argumento
			log(3, "binary parse: DEL %s %d", toks[0], lens[0]);
			res = cache_del(cache, BIN_MODE, toks[0], lens[0]); 
      answer_client(fd, res);
			break;
		
		case GET:
			ntoks = bin_parser(fd, toks, lens, 1); // consumiremos 1 argumento
			log(3, "binary parse: GET %s %d", toks[0], toks[1]);
			char* val;
			unsigned vlen;
			res = cache_get(cache, BIN_MODE, toks[0], lens[0], &val, &vlen);
      answer_client(fd, res);
			break;

		case STATS:
			log(3, "binary parse: STATS");
			struct Stats stats_buf;
			enum code res = cache_stats(cache, BIN_MODE, &stats_buf);
      answer_client(fd, res);
			break;
    }
		break;
	}
	return 0;
}

int bin_parser(int fd, char *toks[], int *lens , int ntoks) {
	int len;
	for (int i = 0; i < ntoks; i++) {
		read(fd, &len, 4); // se lee la longitud del argumento
    len = ntohl(len);
	  toks[i] = malloc(len);
		read(fd, toks[i],len); // se lee el argumento 
    lens[i] = len;
	}
	return ntoks;
}
