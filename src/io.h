#ifndef __IO_H__
#define __IO_H__

#include "memcached.h"

/* Macro interna */
#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))	\
		return 0;						\
	if (rc <= 0)							\
		return -1;						\
	rc; })

//! @brief Retorna una cadena con un formato estandar para estadisticas
//! @param s Conjunto de estadisticas
char* format_stats(struct stats s);

//! @brief Uso del programa. Termina la ejecución de este
void usage();

#endif