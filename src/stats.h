#ifndef __STATS_H__
#define __STATS_H__

#include <stdint.h>

struct Stats {
  uint64_t get, put, del, keys;
};

//! @brief Crea una estructura de estadisticas
struct Stats stats_init();

void stats_inc_get(struct Stats *s);

void stats_inc_put(struct Stats *s);

void stats_inc_del(struct Stats *s);

void stats_inc_keys(struct Stats *s);

void stats_dec_keys(struct Stats *s);

//! @brief Retorna una cadena con un formato estandar para estadisticas
int format_stats(struct Stats *s, char buf[], unsigned n);

#endif