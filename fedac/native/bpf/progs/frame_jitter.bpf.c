/* frame_jitter.bpf.c — DRM page flip latency spike detector.
 * Attaches kprobe/kretprobe to drm_atomic_helper_commit to measure
 * commit duration. Events exceeding threshold go to a ring buffer. */

#include "../include/vmlinux_defs.h"

#define JITTER_THRESH_NS  20000000ULL  /* 20ms — a 60fps frame is 16.67ms */

struct flip_event {
    __u64 ts_ns;
    __u64 duration_ns;
    __u32 pid;
    char  comm[16];
};

/* Per-CPU stash for kprobe entry timestamp */
struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __uint(max_entries, 256);
    __type(key, __u64);   /* pid_tgid */
    __type(value, __u64); /* entry timestamp */
} flip_start SEC(".maps");

/* Ring buffer for userspace consumption */
struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 64 * 1024); /* 64KB ring */
} events SEC(".maps");

SEC("kprobe/drm_atomic_helper_commit")
int kp_drm_commit(struct pt_regs *ctx) {
    __u64 pid_tgid = bpf_get_current_pid_tgid();
    __u64 ts = bpf_ktime_get_ns();
    bpf_map_update_elem(&flip_start, &pid_tgid, &ts, BPF_ANY);
    return 0;
}

SEC("kretprobe/drm_atomic_helper_commit")
int krp_drm_commit(struct pt_regs *ctx) {
    __u64 pid_tgid = bpf_get_current_pid_tgid();
    __u64 *tsp = bpf_map_lookup_elem(&flip_start, &pid_tgid);
    if (!tsp) return 0;

    __u64 duration = bpf_ktime_get_ns() - *tsp;
    if (duration < JITTER_THRESH_NS) return 0;

    struct flip_event *e = bpf_ringbuf_reserve(&events,
        sizeof(struct flip_event), 0);
    if (!e) return 0;

    e->ts_ns = bpf_ktime_get_ns();
    e->duration_ns = duration;
    e->pid = pid_tgid >> 32;
    bpf_get_current_comm(e->comm, sizeof(e->comm));
    bpf_ringbuf_submit(e, 0);
    return 0;
}

LICENSE;
