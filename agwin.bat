@echo off
gccwin -S autogen.c -o autogen.S -I../pdos/pdpclib
aswin -o autogen.o autogen.S
ldwin -s -o autogen.exe ../pdos/pdpclib/w32start.o autogen.o ../pdos/pdpclib/msvcrt.a
autogen gccwin
