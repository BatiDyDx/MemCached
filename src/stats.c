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

int format_stats(struct Stats *s, char buf[], unsigned n) {
  log(4, "Format of stats");
  int len = snprintf(buf, n, "OK PUTS=%lu DELS=%lu GETS=%lu KEYS=%lu\n",
            s->put, s->del, s->get, s->keys);
  return len;
}
