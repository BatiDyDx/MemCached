#ifndef __BIN_PROC_H__
#define __BIN_PROC_H__

#define BIN_MAX_TOKS 3

//! @brief Funcion de parseo binario. Recibe un file descriptor 
//! y una cantidad de argumentos (tokens) a leer. Retorna los
//! tokens en el array 'toks' y sus respectivas longitudes en 'toks_len' 
//!
//! @param[in] fd - file descriptor.
//! @param[out] toks - tokens del comando
//! @param[out] toks_len - longitudes de los tokens del comando
//! @param[in] ntoks - cantidad de tokens a recuperar.
int bin_parser(int fd, char *toks[], int *toks_len , int ntoks);


int bin_handler(int fd);

#endif