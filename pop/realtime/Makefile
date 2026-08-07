CC ?= clang
CFLAGS ?= -std=c11 -O2 -Wall -Wextra -Werror

.PHONY: all run clean

all: c/measure-engine

c/measure-engine: c/measure-engine.c
	$(CC) $(CFLAGS) $< -o $@ -framework AudioToolbox -framework CoreFoundation -lm -lpthread

run: all
	node plan-agent.mjs

many: all
	node render-many.mjs

clean:
	unlink c/measure-engine 2>/dev/null || true
