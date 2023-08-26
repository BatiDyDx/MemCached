bin_processing.o: src/bin_processing.c src/cache.h src/common.h src/log.h \
 src/lru.h src/ll.h src/stats.h src/io.h src/client_data.h src/dalloc.h \
 src/memcached.h src/bin_processing.h
cache.o: src/cache.c src/common.h src/log.h src/stats.h src/cache.h \
 src/lru.h src/ll.h src/dalloc.h
client_data.o: src/client_data.c src/client_data.h src/dalloc.h \
 src/cache.h src/common.h src/log.h src/lru.h src/ll.h src/stats.h \
 src/io.h
common.o: src/common.c src/common.h src/log.h
dalloc.o: src/dalloc.c src/dalloc.h src/cache.h src/common.h src/log.h \
 src/lru.h src/ll.h src/stats.h src/memcached.h
io.o: src/io.c src/io.h src/client_data.h src/dalloc.h src/cache.h \
 src/common.h src/log.h src/lru.h src/ll.h src/stats.h
ll.o: src/ll.c src/cache.h src/common.h src/log.h src/lru.h src/ll.h \
 src/stats.h src/dalloc.h
log.o: src/log.c src/log.h
lru.o: src/lru.c src/cache.h src/common.h src/log.h src/lru.h src/ll.h \
 src/stats.h src/dalloc.h
memcached.o: src/memcached.c src/dalloc.h src/cache.h src/common.h \
 src/log.h src/lru.h src/ll.h src/stats.h src/memcached.h \
 src/text_processing.h src/client_data.h src/bin_processing.h src/io.h \
 src/sock.h
sock.o: src/sock.c src/client_data.h src/memcached.h src/cache.h \
 src/common.h src/log.h src/lru.h src/ll.h src/stats.h src/sock.h \
 src/dalloc.h src/io.h
stats.o: src/stats.c src/stats.h src/log.h
text_processing.o: src/text_processing.c src/common.h src/log.h \
 src/memcached.h src/cache.h src/lru.h src/ll.h src/stats.h \
 src/text_processing.h src/client_data.h src/io.h src/dalloc.h
