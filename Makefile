CC = gcc
CFLAGS += -Wall -Wextra -Werror #-O3
LDFLAGS += -pthread
SOURCE = src/bin_processing.o src/cache.o src/common.o src/dalloc.o src/io.o\
	src/ll.o src/log.o src/lru.o src/memcached.o src/stats.o src/text_processing.o

all: memcached bind

memcached: $(SOURCE)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCE) -o memcached

bind: src/bind.o src/sock.o src/log.o
	$(CC) $(CFLAGS) $(LDFLAGS) src/bind.o src/sock.o src/log.o -o server

clean:
	rm -f memcached server src/*.o

run: all
	./memcached

.deps.mk:
	$(CC) -MM src/*.c > .deps.mk

.PHONY: all clean run

include .deps.mk
