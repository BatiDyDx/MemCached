#ifndef __TEXT_PROC_H__
#define __TEXT_PROC_H__

#include "client_data.h"

#define TEXT_MAX_TOKS 3
#define TEXT_LIMIT_SIZE 2048

//! @brief Función de manejo de entrada en modo texto.
int text_handler(struct ClientData *cdata);

//! @brief Parser de texto.
//!
//! @param[in] buf - const char *.  No debe contener el '\n'
//! @param[out] toks - char *: arreglo de tokens
//! @param[out] lens - arreglo de enteros, contiene la longitud de los tokens
enum code text_parser(char *buf, char *toks[TEXT_MAX_TOKS], int lens[TEXT_MAX_TOKS]);

int answer_text_client(int fd, enum code res, char *data, uint64_t len);

#endif      