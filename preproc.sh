#!/bin/sh
gcc -U__GNUC__ -U__linux__ -U__unix__ -DNDEBUG -ansi -E main.c -o ct000.in
awk '!/^#/' ct000.in >ct999.in
cat ct999.in >ct000.in
awk '!/^$/' ct000.in >ct999.in
cat ct999.in >ct000.in
