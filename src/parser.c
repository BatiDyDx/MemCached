#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <endian.h>
#include <math.h>
#include "parser.h"

int text_parser(const char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]) {
	int ntok;
	log(3, "parser(%s)", buf);

	/* Separar tokens */
	{
		char *p = buf;
		ntok = 0;
		toks[ntok++] = p;
		while (ntok < TEXT_MAX_TOKS && (p = strchrnul(p, ' ')) && *p) {
			/* Longitud token anterior */
			lens[ntok-1] = p - toks[ntok-1];
			*p++ = 0;
			/* Comienzo nueva token */
			toks[ntok++] = p;
		}
		lens[ntok-1] = p - toks[ntok-1];
	}

	log(3, "checking '%s', ntok = %i", toks[0], ntok);
	return ntok;
}

unsigned char* get_arg(unsigned char* buf) {
	int len;
	memmove(&len, buf,4);
	len = ntohl(len);
	buf = buf + 4;

	unsigned char* arg = malloc(len);
	assert(arg);
	arg = memmove(arg, buf, len);

	return arg;
}


int bin_parser(unsigned char *buf, char* toks[TEXT_MAX_TOKS]) {
	unsigned char* p = buf;
	unsigned char* command = p;
	p++;
	int ntok = 0;
	switch((int) *command) {
		case 11: // PUT
			toks[ntok++] = command;
			unsigned char* arg1 = get_arg(p);
			toks[ntok++] = arg1;

			int len;
			memmove(&len, p,4);
			len = ntohl(len);
			p = p + (4 + len);

			unsigned char* arg2 = get_arg(p);
			toks[ntok++] = arg2;
			printf("PUT %s %s\n",arg1, arg2);
			break;

		case 12 : //DEL
			toks[ntok++] = "DEL";
			unsigned char* arg = get_arg(p);
			toks[ntok++] = arg;
			printf("DEL %s\n",arg);
			break;

		case 13: // GET
			toks[ntok++] = "GET";
			arg = get_arg(p);
			toks[ntok++] = arg;
			printf("GET %s\n",arg);
			break;

		case 21: // STATS
			toks[ntok++] = "STATS";
			printf("STATS \n");
			break;
			
		default:
			printf("Command not recognized\n");
			return -1;
	}
	return ntok;
}