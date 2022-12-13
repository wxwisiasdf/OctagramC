CC = gcc
CFLAGS = -std=c99 -DANSI_COLOUR=1 -Wall -Wextra \
	-pedantic -Wno-unused-parameter -Ofast -g

all: build

run: build
	valgrind --leak-check=full --track-origins=yes ./occ hello.in -370 -print-ast
#	gdb --args ./occ stt.c -370 -print-ast

build: occ

clean:
	$(RM) *.o
	$(RM) occ

.PHONY: build all

# ==============================================================================
occ: as386.o diag.o ast.o optzer.o constevl.o lexer.o parser.o main.o util.o \
	graphviz.c backend.o mf370.o
	$(CC) $(CFLAGS) $^ -o $@

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
