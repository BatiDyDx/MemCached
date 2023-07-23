#include <arpa/inet.h>
#include "common.h"
#include "log.h"
#include "bin_processing.h"

static int get_args(unsigned char* buf, unsigned long buf_size,
                    char *toks[], unsigned toks_len[], unsigned ntoks) {
	int len;
  unsigned long rem_buf = buf_size;
	for (int i = 0; i < ntoks; i++) {
    memmove(&len, buf, 4);
    len = ntohl(len);
    buf = buf + 4;
    rem_buf -= 4;
    if (rem_buf < len)
      return i;
    toks[i] = buf;
    buf = buf + len;
    rem_buf -= len;
    toks_len[i] = len;
  }
	return ntoks;
}

int bin_parser(unsigned char *buf, char* toks[BIN_MAX_TOKS], int lens[BIN_MAX_TOKS]) {
	unsigned char* p = buf;
	int ntok = 0;
	p++;
  toks[ntok++] = code_str(*buf);
  get_args(buf + 1, );
  switch((int) *buf) {
		case PUT:
			log(3, "binary parse: PUT %s %s", arg1, arg2);
			break;

		case DEL:
			toks[ntok++] = "DEL";
			unsigned char* arg = get_arg(p);
			toks[ntok++] = arg;
			log(3, "binary parse: DEL %s", arg);
			break;

		case GET:
			toks[ntok++] = "GET";
			arg = get_arg(p);
			toks[ntok++] = arg;
			log(3, "binary parse: GET %s", arg);
			break;

		case STATS:
			toks[ntok++] = "STATS";
			log(3, "binary parse: STATS");
			break;
			
		default:
			printf("Command not recognized\n");
			return -1;
	}
	return ntok;
}
