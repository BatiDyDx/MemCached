#include "client_data.h"
#include "dalloc.h"
#include "io.h"

struct ClientData* cdata_init(int csock, int mode) {
  struct ClientData* cdata = dalloc(sizeof(struct ClientData));
	cdata->buf_size = 3 * MAX_READ_SIZE;
	cdata->buffer = dalloc(sizeof(cdata->buf_size));
	cdata->client_fd = csock;
	cdata->mode = mode;
	cdata->current_idx = 0;
  return cdata;
} 

void reset_client_info(struct ClientData* cdata) {
  free(cdata->buffer);
  cdata->current_idx = 0;
  cdata->buf_size = 3 * MAX_READ_SIZE;
  cdata->buffer = dalloc(cdata->buf_size);
}
