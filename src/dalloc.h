#ifndef __DALLOC_H__
#define __DALLOC_H__

#include <stdlib.h>

//! @brief Interfaz de malloc que desaloja de la cache cuando no hay memoria
//! disponible.
//! @return Puntero a memoria alojada. Si retorna NULL, entonces no se ha podido
//! obtener suficiente memoria incluso tras desalojar toda la cache
void* dalloc(size_t size);

//! @brief Interfaz para funcion similar a realloc, intenta conseguir una region
//! de memoria incrementada en inc cantidad de bytes.
//! @return Puntero a memoria alojada. Si retorna NULL, entonces no se ha podido
//! obtener suficiente memoria incluso tras desalojar toda la cache
void* drealloc(void* ptr, size_t size, size_t inc);

#endif