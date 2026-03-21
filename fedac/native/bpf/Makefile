# BPF trace tools for AC Native OS
#
# Two build modes:
#   1. Full BPF (HAS_LIBBPF=1): compiles BPF bytecode with clang, links libbpf
#   2. Ftrace fallback (default): ac-trace uses tracefs directly, no BPF deps
#
# The ftrace fallback ships by default and works with the enabled kernel config.
# Full BPF mode requires: clang, libbpf-devel, libelf-devel, zlib-devel

CC       ?= musl-gcc
CLANG    ?= clang
CFLAGS   := -O2 -Wall -Wextra -std=gnu11
LDFLAGS  := -static

BPF_CFLAGS := -target bpf -O2 -g -D__TARGET_ARCH_x86

PROGS    := progs/syscall_profile.bpf.o \
            progs/frame_jitter.bpf.o \
            progs/alsa_xrun.bpf.o \
            progs/input_latency.bpf.o \
            progs/alloc_profile.bpf.o

SKEL_DIR := loaders/skel

.PHONY: all clean bpf-progs

all: ac-trace

# --- Ftrace-only build (default, no BPF dependencies) ---
ac-trace: loaders/ac-trace.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<
	@echo "Built ac-trace (ftrace mode, $$(du -sh $@ | cut -f1))"

# --- Full BPF build (opt-in: make bpf-full HAS_LIBBPF=1) ---
.PHONY: bpf-full
bpf-full: bpf-progs ac-trace-bpf

bpf-progs: $(PROGS)

progs/%.bpf.o: progs/%.bpf.c include/vmlinux_defs.h
	$(CLANG) $(BPF_CFLAGS) -I include -c -o $@ $<

# Generate skeleton headers from compiled BPF objects
$(SKEL_DIR)/%.skel.h: progs/%.bpf.o
	@mkdir -p $(SKEL_DIR)
	xxd -i $< > $@

ac-trace-bpf: loaders/ac-trace.c $(patsubst progs/%.bpf.o,$(SKEL_DIR)/%.skel.h,$(PROGS))
	$(CC) $(CFLAGS) $(LDFLAGS) -DHAS_LIBBPF -I $(SKEL_DIR) -o ac-trace $< -lbpf -lelf -lz
	@echo "Built ac-trace (BPF mode, $$(du -sh ac-trace | cut -f1))"

clean:
	rm -f ac-trace progs/*.bpf.o $(SKEL_DIR)/*.skel.h
	@echo "Cleaned BPF build artifacts"
