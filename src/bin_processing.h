#ifndef __BIN_PROC_H__
#define __BIN_PROC_H__

#define BIN_MAX_TOKS 3

//! @brief 
//!
//! @param[in] fd - file descriptor.
//! @param[out] toks
int bin_parser(int fd, char *toks[], int *toks_len , int ntoks);

int bin_handler(int fd);

#endif