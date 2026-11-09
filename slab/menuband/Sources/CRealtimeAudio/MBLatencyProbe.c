#include "MBRealtimeQueue.h"
#include <stdatomic.h>
#include <stdlib.h>

_Static_assert(ATOMIC_LLONG_LOCK_FREE == 2, "timing indices must be lock-free");

#define LATENCY_CAPACITY 256u
struct MBLatencyProbe {
    _Atomic unsigned long long in_read, in_write, out_read, out_write, drops;
    MBLatencySample input[LATENCY_CAPACITY], output[LATENCY_CAPACITY];
    MBLatencySample rendering[LATENCY_CAPACITY];
    unsigned rendering_count; // render thread only
};
MBLatencyProbe *mb_latency_create(void) {
    MBLatencyProbe *p = calloc(1, sizeof(*p));
    if (!p) return NULL;
    atomic_init(&p->in_read, 0); atomic_init(&p->in_write, 0);
    atomic_init(&p->out_read, 0); atomic_init(&p->out_write, 0);
    atomic_init(&p->drops, 0);
    return p;
}
void mb_latency_destroy(MBLatencyProbe *p) { free(p); }
bool mb_latency_stage(MBLatencyProbe *p, MBLatencySample s) {
    unsigned long long w = atomic_load_explicit(&p->in_write, memory_order_relaxed);
    unsigned long long r = atomic_load_explicit(&p->in_read, memory_order_acquire);
    if (w - r >= LATENCY_CAPACITY) {
        atomic_fetch_add_explicit(&p->drops, 1, memory_order_relaxed);
        return false;
    }
    p->input[w % LATENCY_CAPACITY] = s;
    atomic_store_explicit(&p->in_write, w + 1, memory_order_release);
    return true;
}
void mb_latency_begin_render(MBLatencyProbe *p) {
    // An abandoned/error block must not turn into a successful measurement.
    if (p->rendering_count) atomic_fetch_add_explicit(&p->drops, p->rendering_count, memory_order_relaxed);
    unsigned long long r = atomic_load_explicit(&p->in_read, memory_order_relaxed);
    unsigned long long w = atomic_load_explicit(&p->in_write, memory_order_acquire);
    p->rendering_count = 0;
    while (r != w && p->rendering_count < LATENCY_CAPACITY) {
        p->rendering[p->rendering_count++] = p->input[r++ % LATENCY_CAPACITY];
    }
    atomic_store_explicit(&p->in_read, r, memory_order_release);
}
void mb_latency_end_render(MBLatencyProbe *p, double now, bool valid) {
    unsigned long long w = atomic_load_explicit(&p->out_write, memory_order_relaxed);
    unsigned long long r = atomic_load_explicit(&p->out_read, memory_order_acquire);
    for (unsigned i = 0; i < p->rendering_count; ++i) {
        MBLatencySample s = p->rendering[i];
        if (!valid || w - r >= LATENCY_CAPACITY || !(s.event > 0 && s.event <= s.captured &&
            s.captured <= s.queued && s.queued <= s.staged && s.staged <= now)) {
            atomic_fetch_add_explicit(&p->drops, 1, memory_order_relaxed);
            continue;
        }
        s.rendered = now;
        p->output[w++ % LATENCY_CAPACITY] = s;
    }
    atomic_store_explicit(&p->out_write, w, memory_order_release);
    p->rendering_count = 0;
}
bool mb_latency_pop(MBLatencyProbe *p, MBLatencySample *s) {
    unsigned long long r = atomic_load_explicit(&p->out_read, memory_order_relaxed);
    unsigned long long w = atomic_load_explicit(&p->out_write, memory_order_acquire);
    if (r == w) return false;
    *s = p->output[r % LATENCY_CAPACITY];
    atomic_store_explicit(&p->out_read, r + 1, memory_order_release);
    return true;
}
uint64_t mb_latency_take_drops(MBLatencyProbe *p) {
    return atomic_exchange_explicit(&p->drops, 0, memory_order_relaxed);
}
