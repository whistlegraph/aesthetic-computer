/* Minimal kernel struct definitions for BPF programs targeting AC Native OS.
 * Hand-extracted for the pinned kernel version (6.14.x / 6.19.x, x86_64).
 * No BTF/CO-RE — struct layouts are fixed to our kernel build. */

#ifndef __VMLINUX_DEFS_H
#define __VMLINUX_DEFS_H

typedef unsigned char        __u8;
typedef unsigned short       __u16;
typedef unsigned int         __u32;
typedef unsigned long long   __u64;
typedef signed char          __s8;
typedef signed short         __s16;
typedef signed int           __s32;
typedef signed long long     __s64;

#define SEC(NAME) __attribute__((section(NAME), used))
#define __uint(name, val)  int (*name)[val]
#define __type(name, val)  typeof(val) *name

enum bpf_map_type {
    BPF_MAP_TYPE_HASH             = 1,
    BPF_MAP_TYPE_ARRAY            = 2,
    BPF_MAP_TYPE_PERF_EVENT_ARRAY = 4,
    BPF_MAP_TYPE_PERCPU_HASH      = 5,
    BPF_MAP_TYPE_PERCPU_ARRAY     = 6,
    BPF_MAP_TYPE_RINGBUF          = 27,
};

/* Tracepoint context for raw_syscalls:sys_enter / sys_exit */
struct trace_event_raw_sys_enter {
    __u64 __pad0;
    __s64 id;
    __u64 args[6];
};

struct trace_event_raw_sys_exit {
    __u64 __pad0;
    __s64 id;
    __s64 ret;
};

/* x86_64 pt_regs for kprobe context */
struct pt_regs {
    __u64 r15, r14, r13, r12;
    __u64 bp, bx;
    __u64 r11, r10, r9, r8;
    __u64 ax, cx, dx, si, di;
    __u64 orig_ax;
    __u64 ip, cs, flags, sp, ss;
};

/* BPF helper declarations */
static void *(*bpf_map_lookup_elem)(void *map, const void *key)
    = (void *)1;
static long (*bpf_map_update_elem)(void *map, const void *key,
    const void *value, __u64 flags)
    = (void *)2;
static __u64 (*bpf_ktime_get_ns)(void)
    = (void *)5;
static __u64 (*bpf_get_current_pid_tgid)(void)
    = (void *)14;
static long (*bpf_get_current_comm)(void *buf, __u32 size)
    = (void *)16;
static long (*bpf_perf_event_output)(void *ctx, void *map,
    __u64 flags, void *data, __u64 size)
    = (void *)25;
static void *(*bpf_ringbuf_reserve)(void *ringbuf, __u64 size, __u64 flags)
    = (void *)131;
static void (*bpf_ringbuf_submit)(void *data, __u64 flags)
    = (void *)132;

#define BPF_ANY     0
#define BPF_NOEXIST 1
#define BPF_EXIST   2

#define LICENSE SEC("license") char _license[] = "GPL"

#endif
