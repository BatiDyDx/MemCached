#ifndef __TEXT_PROC_H__
#define __TEXT_PROC_H__

#define TEXT_MAX_TOKS 3
#define TEXT_BUF_SIZE 2048

//! @brief Función de manejo de entrada en modo texto.
int text_handler(int fd);

//! @brief Parser de texto.
//!
//! @param[in] buf - const char *.  No debe contener el '\n'
//! @param[out] toks - char *: arreglo de tokens
//! @param[out] lens - arreglo de enteros, contiene la longitud de los tokens
enum code text_parser(unsigned char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]);


#endif      