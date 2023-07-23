#ifndef __TEXT_PROC_H__
#define __TEXT_PROC_H__

#define TEXT_MAX_TOKS 3
#define TEXT_BUF_SIZE 2048

int text_parser(const char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]);

int text_handler(int fd);

#endif