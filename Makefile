CC = gcc
CFLAGS = -std=c99 -DANSI_COLOUR=1 -Wall -Wextra \
	-pedantic -Wno-unused-parameter -O0 -g

all: build

run: build
	valgrind --leak-check=full --track-origins=yes ./occ hello.in -370 -print-ast
#	gdb --args ./occ hello.in -370 -print-ast

build: occ

clean:
	$(RM) *.o
	$(RM) occ

.PHONY: build all

# ==============================================================================
occ: diag.o ast.o optzer.o constevl.o lexer.o parser.o main.o util.o \
	graphviz.c mf370.o as386.o ssa.o partyp.o parexpr.o
	$(CC) $(CFLAGS) $^ -o $@

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
