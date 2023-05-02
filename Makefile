CC = gcc
CFLAGS = -std=c99 -DTARGET_AS386=1 -DANSI_COLOUR=1 -Wall -Wextra -pedantic \
	-Wno-unused-parameter -pipe -g -fsanitize=undefined
all: build

run: all
#	valgrind --leak-check=full --track-origins=yes ./occ ct000.in -print-ast
	gdb --args ./occ ct000.in -print-ast -o ct000.out

test: all
	./occ tests/t00.c -print-ast
	./occ tests/t01.c -print-ast
	./occ tests/t02.c -print-ast
	./occ tests/t03.c -print-ast
	./occ tests/t04.c -print-ast
	./occ tests/t05.c -print-ast

build: occ

clean:
	$(RM) occ

.PHONY: build all clean
occ: main.c as386.c ast.c constevl.c diag.c lexer.c mf370.c optzer.c \
	parexpr.c parser.c partyp.c ssa.c util.c \
	as386.h ast.h constevl.h diag.h lexer.h mf370.h optzer.h parexpr.h \
	parser.h partyp.h ssa.h util.h stdbool.h context.h
	$(CC) $(CFLAGS) $< -o $@
