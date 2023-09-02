bin_protocol.o: src/bin_protocol.c src/cache.h src/common.h src/log.h \
 src/lru.h src/ll.h src/stats.h src/io.h src/dalloc.h src/memcached.h \
 src/bin_protocol.h src/client_data.h
cache.o: src/cache.c src/common.h src/log.h src/stats.h src/cache.h \
 src/lru.h src/ll.h src/dalloc.h
client_data.o: src/client_data.c src/client_data.h src/common.h src/log.h \
 src/io.h src/dalloc.h
common.o: src/common.c src/common.h src/log.h
dalloc.o: src/dalloc.c src/dalloc.h src/memcached.h src/cache.h \
 src/common.h src/log.h src/lru.h src/ll.h src/stats.h
io.o: src/io.c src/io.h src/dalloc.h src/log.h
ll.o: src/ll.c src/cache.h src/common.h src/log.h src/lru.h src/ll.h \
 src/stats.h src/dalloc.h
log.o: src/log.c src/log.h
lru.o: src/lru.c src/cache.h src/common.h src/log.h src/lru.h src/ll.h \
 src/stats.h src/dalloc.h
memcached.o: src/memcached.c src/dalloc.h src/memcached.h src/cache.h \
 src/common.h src/log.h src/lru.h src/ll.h src/stats.h \
 src/text_protocol.h src/client_data.h src/io.h src/bin_protocol.h \
 src/sock.h
sock.o: src/sock.c src/client_data.h src/common.h src/log.h src/io.h \
 src/memcached.h src/cache.h src/lru.h src/ll.h src/stats.h src/sock.h \
 src/dalloc.h
stats.o: src/stats.c src/stats.h src/log.h
text_protocol.o: src/text_protocol.c src/common.h src/log.h \
 src/memcached.h src/cache.h src/lru.h src/ll.h src/stats.h \
 src/text_protocol.h src/client_data.h src/io.h src/dalloc.h
