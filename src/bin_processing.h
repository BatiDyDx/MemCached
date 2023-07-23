#ifndef __BIN_PROC_H__
#define __BIN_PROC_H__

#define BIN_MAX_TOKS 3

int bin_parser(const char *buf, char *toks[BIN_MAX_TOKS], int lens[BIN_MAX_TOKS]);

int bin_consume(int fd, char buf[TEXT_BUF_SIZE], int blen);

int bin_handler(int fd);

#endif