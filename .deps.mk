bin_processing.o: src/bin_processing.c src/common.h src/log.h src/io.h \
 src/memcached.h src/bin_processing.h
cache.o: src/cache.c src/stats.h src/cache.h src/common.h src/log.h \
 src/lru.h src/ll.h
common.o: src/common.c src/common.h src/log.h
io.o: src/io.c src/io.h src/memcached.h
ll.o: src/ll.c src/cache.h src/common.h src/log.h src/lru.h src/ll.h
log.o: src/log.c src/log.h
lru.o: src/lru.c src/cache.h src/common.h src/log.h src/lru.h src/ll.h
memcached.o: src/memcached.c src/memcached.h src/sock.h src/common.h \
 src/log.h src/text_processing.h src/bin_processing.h src/cache.h \
 src/lru.h src/io.h
sock.o: src/sock.c src/sock.h src/common.h src/log.h
stats.o: src/stats.c src/stats.h src/log.h
text_processing.o: src/text_processing.c src/common.h src/log.h \
 src/text_processing.h src/io.h src/memcached.h src/cache.h src/lru.h
