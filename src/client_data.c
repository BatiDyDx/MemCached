#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include "client_data.h"
#include "dalloc.h"
#include "bin_protocol.h"
#include "text_protocol.h"

struct ClientData* listen_data_init(int lsock) {
  struct ClientData* cdata = malloc(sizeof(struct ClientData));
  assert(cdata);
  cdata->fd = lsock;
  return cdata;
}

struct ClientData* client_data_init(int csock, int mode) {
  struct ClientData* cdata = dalloc(sizeof(struct ClientData));
  if (!cdata)
    return NULL;
	cdata->buf_size = BUFFER_SIZE;
	cdata->buffer = dalloc(cdata->buf_size);
  if (!cdata->buffer) { // No se pudo alocar buffer
    free(cdata);
    return NULL;
  }
	cdata->fd = csock;
	cdata->mode = mode;
	cdata->current_idx = 0;
  return cdata;
} 

enum IO_STATUS_CODE client_fill_buffer(struct ClientData *cdata) {
  long rb;
  int stop = 0;
  for (int i = 0; i < 10 && !stop; i++) {
    if (cdata->current_idx + READ_SIZE > cdata->buf_size){
      if (client_increase_buffer(cdata) < 0) {
        return ERROR;
      }
    }

    rb = read(cdata->fd, cdata->buffer + cdata->current_idx, READ_SIZE);
    log(3, "Leidos %d bytes de fd %d", rb, cdata->fd);
    if (rb < READ_SIZE)
      stop = 1;
    if (rb > 0)
      cdata->current_idx += rb;
  }
	if (rb < 0 && (errno != EAGAIN && errno != EWOULDBLOCK))
		return ERROR;
	if (rb == 0)
		return CLOSED;
  return IO_OK;
}

int client_increase_buffer(struct ClientData *cdata) {
  log(2, "Realloc de buffer para fd %d", cdata->fd);
  cdata->buffer = drealloc(cdata->buffer, cdata->buf_size, BUFFER_SIZE);
  cdata->buf_size += BUFFER_SIZE;
  if (!cdata->buffer) {
    if(cdata->mode == TEXT_MODE) 
      answer_text_client(cdata->fd, EOOM, NULL, 0);
    else
      answer_bin_client(cdata->fd, EOOM, NULL, 0);
    return -1;
  }
  return 0;
}

void client_close_connection(struct ClientData *cdata) {
  log(1, "Cierre de conexion con el fd: %d", cdata->fd);
  close(cdata->fd);
  free(cdata->buffer);
  free(cdata);
}
