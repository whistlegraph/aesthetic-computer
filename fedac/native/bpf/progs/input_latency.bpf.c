/* input_latency.bpf.c — evdev input event kernel-side timestamping.
 * Attaches kprobe on input_event() to capture the kernel timestamp of each
 * input event. Userspace compares with its own CLOCK_MONOTONIC read to
 * compute kernel→userspace latency. */

#include "../include/vmlinux_defs.h"

/* Histogram: 64 buckets, each 100us wide (0-6.4ms range) */
#define HIST_BUCKETS 64
#define BUCKET_US    100

struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __uint(max_entries, HIST_BUCKETS);
    __type(key, __u32);
    __type(value, __u64);
} latency_hist SEC(".maps");

/* Last N events ring buffer for detailed inspection */
struct input_ev {
    __u64 kernel_ts_ns;
    __u32 type;   /* EV_KEY, EV_ABS, etc. */
    __u32 code;
    __s32 value;
    __u32 pid;
};

struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 32 * 1024);
} events SEC(".maps");

/* kprobe on input_event(struct input_dev *dev, uint type, uint code, int val)
 * Arguments: di=dev, si=type, dx=code, cx=value (x86_64 calling convention) */
SEC("kprobe/input_event")
int kp_input_event(struct pt_regs *ctx) {
    __u64 ts = bpf_ktime_get_ns();
    __u32 type = (__u32)ctx->si;
    __u32 code = (__u32)ctx->dx;
    __s32 value = (__s32)ctx->cx;

    /* Only trace EV_KEY (1) and EV_ABS (3) — skip EV_SYN noise */
    if (type != 1 && type != 3) return 0;

    struct input_ev *e = bpf_ringbuf_reserve(&events,
        sizeof(struct input_ev), 0);
    if (!e) return 0;

    e->kernel_ts_ns = ts;
    e->type = type;
    e->code = code;
    e->value = value;
    e->pid = bpf_get_current_pid_tgid() >> 32;
    bpf_ringbuf_submit(e, 0);
    return 0;
}

LICENSE;
