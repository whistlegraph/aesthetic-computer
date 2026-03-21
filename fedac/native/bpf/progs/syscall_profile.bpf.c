/* syscall_profile.bpf.c — Per-syscall invocation count + latency histogram.
 * Attaches to raw_syscalls:sys_enter and sys_exit tracepoints. */

#include "../include/vmlinux_defs.h"

/* Per-CPU map: syscall_nr → entry timestamp (for latency calculation) */
struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __uint(max_entries, 1024);
    __type(key, __u64);   /* pid_tgid */
    __type(value, __u64); /* entry timestamp ns */
} start_ts SEC(".maps");

/* Aggregation: syscall_nr → {count, total_ns} */
struct sc_stat {
    __u64 count;
    __u64 total_ns;
};

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __uint(max_entries, 512);
    __type(key, __s64);           /* syscall number */
    __type(value, struct sc_stat);
} stats SEC(".maps");

SEC("tracepoint/raw_syscalls/sys_enter")
int tp_sys_enter(struct trace_event_raw_sys_enter *ctx) {
    __u64 pid_tgid = bpf_get_current_pid_tgid();
    __u64 ts = bpf_ktime_get_ns();
    bpf_map_update_elem(&start_ts, &pid_tgid, &ts, BPF_ANY);
    return 0;
}

SEC("tracepoint/raw_syscalls/sys_exit")
int tp_sys_exit(struct trace_event_raw_sys_exit *ctx) {
    __u64 pid_tgid = bpf_get_current_pid_tgid();
    __u64 *tsp = bpf_map_lookup_elem(&start_ts, &pid_tgid);
    if (!tsp) return 0;

    __u64 delta = bpf_ktime_get_ns() - *tsp;
    __s64 nr = ctx->id;

    struct sc_stat *s = bpf_map_lookup_elem(&stats, &nr);
    if (s) {
        __sync_fetch_and_add(&s->count, 1);
        __sync_fetch_and_add(&s->total_ns, delta);
    } else {
        struct sc_stat init = { .count = 1, .total_ns = delta };
        bpf_map_update_elem(&stats, &nr, &init, BPF_NOEXIST);
    }
    return 0;
}

LICENSE;
