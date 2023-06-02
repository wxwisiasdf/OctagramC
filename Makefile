CC = gcc
CFLAGS = -std=c99 -DTARGET_AS386=1 -DANSI_COLOUR=1 -Wall -Wextra -Wshadow \
	-pedantic -Wno-unused-parameter -pipe -O0 -g
all: build

run: all
#	valgrind --leak-check=full --track-origins=yes ./occ ct000.in -print-ast
	gdb --args ./occ ct000.in

build: occ test

clean:
	$(RM) occ test

.PHONY: build all clean
occ: main.c as386.c ast.c constevl.c diag.c lexer.c mf370.c optzer.c \
	parexpr.c parser.c partyp.c ssa.c util.c \
	as386.h ast.h constevl.h diag.h lexer.h mf370.h optzer.h parexpr.h \
	parser.h partyp.h ssa.h util.h stdbool.h context.h
	$(CC) $(CFLAGS) $< -o $@

test: test.c tests/files.lst
	$(CC) $(CFLAGS) $< -o $@
