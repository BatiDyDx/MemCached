#ifndef __CLIENT_DATA_H__
#define __CLIENT_DATA_H__

#include "io.h"

#define READ_SIZE (1 << 12)
#define BUFFER_SIZE (3 * READ_SIZE)

struct ClientData {
  char *buffer;
  long buf_size;
  long current_idx;
  int fd;
  int mode; 
};

struct ClientData* listen_data_init(int lsock);

struct ClientData* client_data_init(int csock, int mode);

enum IO_STATUS_CODE client_fill_buffer(struct ClientData *cdata);

int client_increase_buffer(struct ClientData *cdata);

void client_reset_info(struct ClientData* cdata);

void client_close_connection(struct ClientData *cdata);

#endif