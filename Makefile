CC = clang
CFLAGS = -ftime-trace -std=c99 -DTARGET_AS386=1 -DANSI_COLOUR=1 -Wall -Wextra -pedantic \
	-Wno-unused-parameter -pipe -O3 -fwhole-program -g -fsanitize=undefined
all: build

run: all
#	valgrind --leak-check=full --track-origins=yes ./occ hello.c -print-ast
	gdb --args ./occ hello.c -print-ast -o hello.s

build: occ

clean:
	$(RM) *.o occ

.PHONY: build all clean
occ: main.o
	$(CC) $(CFLAGS) $^ -o $@
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
