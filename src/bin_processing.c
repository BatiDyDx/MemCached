#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "common.h"
#include "io.h"
#include "log.h"
#include "bin_processing.h"

/* 0: todo ok, continua. -1 errores */
int bin_handler(int fd) {
	uint64_t nread;
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
		int toks_len[2];
		int ntoks;

		switch (op)
		{
		case PUT: 
			ntoks = bin_parser(fd, toks, toks_len, 2); // consumiremos 2 argumentos
			log(3, "binary parse: PUT %s %s", toks[0], toks[1]);
			break;
		
		case DEL: 
			ntoks = bin_parser(fd, toks, toks_len, 1); // cosumiremos 1 argumento
			log(3, "binary parse: DEL %s %s", toks[0], toks[1]);
			break;
		
		case GET:
			ntoks = bin_parser(fd, toks, toks_len, 1); // consumiremos 1 argumento
			log(3, "binary parse: GET %s %s", toks[0], toks[1]);
			break;

		case STATS:
			log(3, "binary parse: STATS");
			break;
    }
		break;
	}
	return 0;
}

int bin_parser(int fd, char *toks[], int *toks_len , int ntoks) {
	int len;
	for (int i = 0; i < ntoks; i++) {
		read(fd, &len, 4); // se lee la longitud del argumento
    len = ntohl(len);
	  toks[i] = malloc(len);
		read(fd, toks[i],len); // se lee el argumento 
    toks_len[i] = len;
	}
	return ntoks;
}
