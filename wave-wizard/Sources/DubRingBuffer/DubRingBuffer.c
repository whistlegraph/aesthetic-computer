#include "DubRingBuffer.h"
#include <stdatomic.h>
#include <stdlib.h>

struct DubRingBuffer {
    float *data;
    size_t capacity;
    _Atomic size_t read_index;
    _Atomic size_t write_index;
};

DubRingBuffer *dub_ring_create(size_t capacity_frames) {
    DubRingBuffer *ring = calloc(1, sizeof(*ring));
    if (!ring) return NULL;
    ring->data = calloc(capacity_frames, sizeof(float));
    if (!ring->data) { free(ring); return NULL; }
    ring->capacity = capacity_frames;
    atomic_init(&ring->read_index, 0);
    atomic_init(&ring->write_index, 0);
    return ring;
}

void dub_ring_destroy(DubRingBuffer *ring) {
    if (!ring) return;
    free(ring->data);
    free(ring);
}

size_t dub_ring_write(DubRingBuffer *ring, const float *samples, size_t frames) {
    if (!ring || !samples) return 0;
    size_t read = atomic_load_explicit(&ring->read_index, memory_order_acquire);
    size_t write = atomic_load_explicit(&ring->write_index, memory_order_relaxed);
    size_t room = ring->capacity - (write - read);
    if (frames > room) frames = room;
    for (size_t i = 0; i < frames; i++) ring->data[(write + i) % ring->capacity] = samples[i];
    atomic_store_explicit(&ring->write_index, write + frames, memory_order_release);
    return frames;
}

size_t dub_ring_read(DubRingBuffer *ring, float *samples, size_t max_frames) {
    if (!ring || !samples) return 0;
    size_t read = atomic_load_explicit(&ring->read_index, memory_order_relaxed);
    size_t write = atomic_load_explicit(&ring->write_index, memory_order_acquire);
    size_t available = write - read;
    if (max_frames > available) max_frames = available;
    for (size_t i = 0; i < max_frames; i++) samples[i] = ring->data[(read + i) % ring->capacity];
    atomic_store_explicit(&ring->read_index, read + max_frames, memory_order_release);
    return max_frames;
}

size_t dub_ring_available(const DubRingBuffer *ring) {
    if (!ring) return 0;
    size_t read = atomic_load_explicit(&ring->read_index, memory_order_acquire);
    size_t write = atomic_load_explicit(&ring->write_index, memory_order_acquire);
    return write - read;
}
