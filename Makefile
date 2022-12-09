CC = gcc
CFLAGS = -D_FORTIFY_SOURCE=2 -Wall -Wextra -Wno-unused-parameter -Og -g

all: build

run: build
#	valgrind --leak-check=full --track-origins=yes ./cc hello.in -o
	gdb --args ./cc hello.in -o
#	./cc hello.in -o

build: cc

clean:
	$(RM) *.o
	$(RM) cc

.PHONY: build all

# ==============================================================================
cc: as386.o diag.o ast.o optzer.o constevl.o lexer.o parser.o main.o util.o \
	graphviz.c backend.o
	$(CC) $(CFLAGS) $^ -o $@

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
