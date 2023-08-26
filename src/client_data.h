#ifndef __CLIENT_DATA_H__
#define __CLIENT_DATA_H__

struct ClientData {
  char *buffer;
  long buf_size;
  long current_idx;
  int client_fd;
  int mode; 
};

struct ClientData* cdata_init(int csock, int mode);

void reset_client_info(struct ClientData* cdata);

#endif