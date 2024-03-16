CC = gcc
CFLAGS += -Wall -Wextra -Werror
LDFLAGS += -pthread
SOURCE_DIR =  src
SOURCE =  bin_protocol.o  \
          cache.o         \
          client_data.o   \
          common.o        \
          dalloc.o        \
          io.o            \
          ll.o            \
          log.o           \
          lru.o           \
          memcached.o     \
          sock.o          \
          stats.o         \
          text_protocol.o

SOURCE := $(patsubst %,$(SOURCE_DIR)/%,$(SOURCE))
PROGRAM = memcached

all: $(PROGRAM)

$(PROGRAM): $(SOURCE)
	@echo ":: Compiling $$(tput bold)$(PROGRAM)$$(tput sgr0)"
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCE) -o $(PROGRAM)

clean:
	@echo ":: Cleaning directories"
	rm -f $(PROGRAM) $(SOURCE)

run: all
	@echo ":: Running $$(tput bold)$(PROGRAM)$$(tput sgr0)"
	./$(PROGRAM)

.deps.mk:
	@echo ":: Writing source files dependencies"
	$(CC) -MM src/*.c > .deps.mk

.PHONY: all clean run

include .deps.mk
