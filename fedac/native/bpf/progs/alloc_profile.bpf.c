/* alloc_profile.bpf.c — Kernel memory allocation size distribution.
 * Attaches kprobes to __kmalloc and kfree to build a histogram of
 * allocation sizes and track total bytes allocated/freed. */

#include "../include/vmlinux_defs.h"

/* Log2 histogram: bucket i covers [2^i, 2^(i+1)) bytes, 0-20 = 1B–1MB+ */
#define HIST_BUCKETS 21

struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __uint(max_entries, HIST_BUCKETS);
    __type(key, __u32);
    __type(value, __u64);
} size_hist SEC(".maps");

/* Totals: index 0 = alloc count, 1 = free count, 2 = alloc bytes */
struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __uint(max_entries, 3);
    __type(key, __u32);
    __type(value, __u64);
} totals SEC(".maps");

static __u32 log2_bucket(__u64 size) {
    __u32 b = 0;
    while (size > 1 && b < HIST_BUCKETS - 1) {
        size >>= 1;
        b++;
    }
    return b;
}

/* __kmalloc(size_t size, gfp_t flags) — di=size on x86_64 */
SEC("kprobe/__kmalloc")
int kp_kmalloc(struct pt_regs *ctx) {
    __u64 size = ctx->di;
    __u32 bucket = log2_bucket(size);

    __u64 *cnt = bpf_map_lookup_elem(&size_hist, &bucket);
    if (cnt) __sync_fetch_and_add(cnt, 1);

    __u32 k0 = 0, k2 = 2;
    __u64 *alloc_cnt = bpf_map_lookup_elem(&totals, &k0);
    if (alloc_cnt) __sync_fetch_and_add(alloc_cnt, 1);
    __u64 *alloc_bytes = bpf_map_lookup_elem(&totals, &k2);
    if (alloc_bytes) __sync_fetch_and_add(alloc_bytes, size);

    return 0;
}

/* kfree(const void *ptr) — just count frees */
SEC("kprobe/kfree")
int kp_kfree(struct pt_regs *ctx) {
    __u32 k1 = 1;
    __u64 *free_cnt = bpf_map_lookup_elem(&totals, &k1);
    if (free_cnt) __sync_fetch_and_add(free_cnt, 1);
    return 0;
}

LICENSE;
