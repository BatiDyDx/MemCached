CC = gcc
CFLAGS += -Wall -Wextra -Werror #-O3
LDFLAGS += -pthread
SOURCE = $(wildcard src/*.c)
SOURCE_O = $(SOURCE:c=o)

all: memcached

memcached: $(SOURCE_O)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCE_O) -o memcached

clean:
	rm -f memcached src/*.o

run: all
	./memcached

.deps.mk:
	$(CC) -MM src/*.c > .deps.mk

.PHONY: all clean run

include .deps.mk
