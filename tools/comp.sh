#!/bin/bash
./rtfm-core out.core -o out.c && gcc RTFM-PT.c -lpthread $(pkg-config --cflags --libs openssl) fmemopen.c -o PTCORE -DTRACE -DTRACE_WS
