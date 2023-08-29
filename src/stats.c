#include <stdio.h>
#include "stats.h"
#include "log.h"

struct Stats stats_init() {
  struct Stats s;
  s.del = 0;
  s.get = 0;
  s.put = 0;
  s.keys = 0;
  return s;
}

void stats_inc_get(struct Stats *s) { s->get++; }

void stats_inc_put(struct Stats *s) { s->put++; }

void stats_inc_del(struct Stats *s) { s->del++; }

void stats_inc_keys(struct Stats *s) { s->keys++; }

void stats_dec_keys(struct Stats *s) { s->keys--; }

int format_stats(struct Stats *s, char buf[], unsigned n) {
  log(4, "Formateado de estadisticas de uso");
  if (!s)
    return -1;
  int len = snprintf(buf, n, "PUTS=%lu DELS=%lu GETS=%lu KEYS=%lu",
            s->put, s->del, s->get, s->keys);
  return len;
}
