#include "MBRealtimeQueue.h"
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

#define MB_CAPACITY 256u
_Static_assert(ATOMIC_LLONG_LOCK_FREE == 2, "audio indices must be lock-free");
_Static_assert(ATOMIC_BOOL_LOCK_FREE == 2, "audio reset must be lock-free");
struct MBRealtimeQueue {
    _Atomic unsigned long long read;
    _Atomic unsigned long long write;
    _Atomic bool reset;
    _Atomic unsigned long long pitch;
    MBRealtimeCommand commands[MB_CAPACITY];
};

MBRealtimeQueue *mb_realtime_queue_create(void) {
    MBRealtimeQueue *q = calloc(1, sizeof(*q));
    if (!q) return NULL;
    atomic_init(&q->read, 0);
    atomic_init(&q->write, 0);
    atomic_init(&q->reset, false);
    atomic_init(&q->pitch, 0);
    mb_realtime_queue_set_pitch(q, 1.0);
    return q;
}
void mb_realtime_queue_destroy(MBRealtimeQueue *q) { free(q); }
uint32_t mb_realtime_queue_capacity(void) { return MB_CAPACITY; }
bool mb_realtime_queue_push(MBRealtimeQueue *q, MBRealtimeCommand command) {
    if (atomic_load_explicit(&q->reset, memory_order_acquire)) return false;
    unsigned long long w = atomic_load_explicit(&q->write, memory_order_relaxed);
    unsigned long long r = atomic_load_explicit(&q->read, memory_order_acquire);
    if (w - r >= MB_CAPACITY) {
        atomic_store_explicit(&q->reset, true, memory_order_release);
        return false;
    }
    q->commands[w % MB_CAPACITY] = command;
    atomic_store_explicit(&q->write, w + 1, memory_order_release);
    return true;
}
bool mb_realtime_queue_pop(MBRealtimeQueue *q, MBRealtimeCommand *command) {
    unsigned long long r = atomic_load_explicit(&q->read, memory_order_relaxed);
    unsigned long long w = atomic_load_explicit(&q->write, memory_order_acquire);
    if (r == w) return false;
    *command = q->commands[r % MB_CAPACITY];
    atomic_store_explicit(&q->read, r + 1, memory_order_release);
    return true;
}
bool mb_realtime_queue_take_reset(MBRealtimeQueue *q) {
    if (!atomic_load_explicit(&q->reset, memory_order_acquire)) return false;
    unsigned long long w = atomic_load_explicit(&q->write, memory_order_acquire);
    atomic_store_explicit(&q->read, w, memory_order_release);
    atomic_store_explicit(&q->reset, false, memory_order_release);
    return true;
}
void mb_realtime_queue_set_pitch(MBRealtimeQueue *q, double pitch) {
    unsigned long long bits;
    _Static_assert(sizeof(bits) == sizeof(pitch), "pitch representation");
    memcpy(&bits, &pitch, sizeof(bits));
    atomic_store_explicit(&q->pitch, bits, memory_order_relaxed);
}
double mb_realtime_queue_pitch(MBRealtimeQueue *q) {
    unsigned long long bits = atomic_load_explicit(&q->pitch, memory_order_relaxed);
    double pitch;
    memcpy(&pitch, &bits, sizeof(pitch));
    return pitch;
}
