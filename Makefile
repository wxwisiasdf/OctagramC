CC = gcc
CFLAGS = -ansi -DTARGET_AS386=1 -DANSI_COLOUR=1 -Wall -Wextra -pedantic \
	-Wno-unused-parameter -O0 -g -fsanitize=undefined
all: build

run: all
#	valgrind --leak-check=full --track-origins=yes ./occ hello.c -print-ast
	gdb --args ./occ hello.c -print-ast

build: occ

clean:
	$(RM) *.o occ

.PHONY: build all clean
occ: diag.o ast.o optzer.o constevl.o lexer.o parser.o main.o util.o \
	mf370.o as386.o ssa.o partyp.o parexpr.o
	$(CC) $(CFLAGS) $^ -o $@
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
