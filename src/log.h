#ifndef __LOG_H
#define __LOG_H

extern int __loglevel;

/* El nivel de logging determina que logs mostrar
 * Nivel 0: Logs desactivados.
 * Nivel 1: Logs de configuracion basica del servidor. Aviso de eventos importantes
    como señales, inicio o cierre de una conexion
 * Nivel 2: Log de acciones o eventos poco usuales como desalojos, realocameniento
    de buffers, etc.
 * Nivel 3: Logs de eventos mas usuales, muestran cuando atienden
    a un cliente, tamaño del mensaje, codigo a responder, etc.
  * Nivel 4: El servidor detalla practicamente todas las operaciones que
    lleva a cabo, dedicado principalmente para debugging.
*/
void set_loglevel(int lvl);

void log_f(char *file, int line, char* fstr, ...);

#ifndef NOLOG
 #define log(l, ...)							\
	do {								\
		int __ll = (l);						\
		if (__loglevel >= __ll)					\
			log_f(__FILE__, __LINE__, __VA_ARGS__);		\
	} while (0)
#else
 #define log(l, ...)
#endif

#endif
