CC ?= cc
CFLAGS ?= -O2 -g
CFLAGS += -std=c11 -Wall -Wextra -Wpedantic $(EXTRA_CFLAGS) $(shell pkg-config --cflags sdl3)
LDLIBS += $(shell pkg-config --libs sdl3) -lm -pthread

.PHONY: all clean test

all: piecefarm-sdl piece-vm-runner

test: piecefarm-sdl
	./piecefarm-sdl --self-test

piece-vm-runner: piece-vm-runner.c piece-vm-native.h
	$(CC) $(CFLAGS) -o $@ piece-vm-runner.c -lm

piece-vm-native.o: piece-vm-runner.c piece-vm-native.h
	$(CC) $(CFLAGS) -DPIECE_VM_LIBRARY -c -o $@ piece-vm-runner.c

piecefarm-sdl: piecefarm-sdl.c font-matrix-chunky8.h piece-vm-native.o
	$(CC) $(CFLAGS) -o $@ piecefarm-sdl.c piece-vm-native.o $(LDLIBS)

clean:
	$(RM) piecefarm-sdl piece-vm-runner piece-vm-native.o
