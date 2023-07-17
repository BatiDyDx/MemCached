CC = gcc
CFLAGS += -Wall -Wextra -Werror #-O3
LDFLAGS += -pthread

all: memcached 

memcached: memcached.o sock.o common.o log.o parser.o 

clean:
	rm -f memcached *.o

run: all
	./memcached

.deps.mk:
	$(CC) -MM *.c > .deps.mk

.PHONY: all clean run

include .deps.mk
