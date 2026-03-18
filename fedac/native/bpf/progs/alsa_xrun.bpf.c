/* alsa_xrun.bpf.c — ALSA underrun (xrun) detector.
 * Attaches to the snd_pcm xrun tracepoint or kprobe on snd_pcm_update_hw_ptr0
 * and emits events when an underrun occurs. */

#include "../include/vmlinux_defs.h"

struct xrun_event {
    __u64 ts_ns;
    __u32 pid;
    char  comm[16];
};

struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 32 * 1024);
} events SEC(".maps");

/* Counter for total xruns (readable from userspace) */
struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __uint(max_entries, 1);
    __type(key, __u32);
    __type(value, __u64);
} xrun_count SEC(".maps");

/* Attach to kprobe on snd_pcm_xrun — fires on every ALSA underrun/overrun */
SEC("kprobe/snd_pcm_xrun")
int kp_snd_pcm_xrun(struct pt_regs *ctx) {
    /* Increment global counter */
    __u32 zero = 0;
    __u64 *cnt = bpf_map_lookup_elem(&xrun_count, &zero);
    if (cnt) __sync_fetch_and_add(cnt, 1);

    struct xrun_event *e = bpf_ringbuf_reserve(&events,
        sizeof(struct xrun_event), 0);
    if (!e) return 0;

    e->ts_ns = bpf_ktime_get_ns();
    e->pid = bpf_get_current_pid_tgid() >> 32;
    bpf_get_current_comm(e->comm, sizeof(e->comm));
    bpf_ringbuf_submit(e, 0);
    return 0;
}

LICENSE;
