#ifndef __BIN_PROC_H__
#define __BIN_PROC_H__

#define BIN_MAX_TOKS 3

//! @brief Función de manejo de entrada en binario. 
int bin_handler(int fd);

//! @brief Parser binario.
//!
//! @param[in] fd - int. file descriptor.
//! @param[out] toks - char *: tokens del comando.
//! @param[out] lens - int *: longitudes de los tokens del comando.
//! @param[in] ntoks - cantidad de tokens.
int bin_parser(int fd, char *toks[], int *lens , int ntoks);


#endif