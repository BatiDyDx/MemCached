CC = gcc
CFLAGS += -Wall -Wextra -Werror #-O3
LDFLAGS += -pthread
SOURCE = src/bin_processing.o src/cache.o src/common.o src/dalloc.o src/io.o\
	src/ll.o src/log.o src/lru.o src/memcached.o src/sock.o src/stats.o src/text_processing.o\
	src/client_data.o

all: memcached

memcached: $(SOURCE)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCE) -o memcached

clean:
	rm -f memcached src/*.o

run: all
	./memcached

docs:
	doxygen doxygen.cfg

.deps.mk:
	$(CC) -MM src/*.c > .deps.mk

.PHONY: all clean run docs

include .deps.mk
