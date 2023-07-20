#ifndef __PARSER_H
#define __PARSER_H 1

#include "common.h"
#define TEXT_MAX_TOKS 3

//! @brief Parser de texto.
//!
//! @param[in] buf - const char *.  No debe contener el '\n'
//! @param[out] toks - char *: arreglo de tokens
//! @param[out] lens - arreglo de enteros, contiene la longitud de los tokens
//! @param[out] ntok - cantidad de tokens
int text_parser(const char *buf, char *toks[3], int *lens );



//! @brief Parser de binario.
//! 
//! @param[in] buf - unsigned char*. 
//! @param[out] toks - char *: arreglo de tokens 
//! @return ntok - cantidad de tokens
int bin_parser(unsigned char *buf, char *toks[3]);


//! @brief Función auxiliar para parser binario. Dado un array de bytes
//! toma los primeros 4 bytes para conseguir la longitud del argumento a recuperar.
//! Luego toma los siguientes n bytes, donde n es la cantidad de bytes que marca la longitud.  
//!
//! @param[in] buf - unsigned char*: array de bytes  
//! @return arg - unsigned char*: argumento leido
unsigned char* get_arg(unsigned char* buf);

#endif
