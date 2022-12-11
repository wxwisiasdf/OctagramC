CC = gcc
CFLAGS = -std=c99 -DANSI_COLOUR=1 -Wall -Wextra \
	-pedantic -Wno-unused-parameter -O0 -g -fsanitize=undefined

all: build

run: build
	valgrind --leak-check=full --track-origins=yes ./cc hello.in -370
#	gdb --args ./cc stt.c -370

build: cc

clean:
	$(RM) *.o
	$(RM) cc

.PHONY: build all

# ==============================================================================
cc: as386.o diag.o ast.o optzer.o constevl.o lexer.o parser.o main.o util.o \
	graphviz.c backend.o mf370.o
	$(CC) $(CFLAGS) $^ -o $@

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
