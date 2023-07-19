#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
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
