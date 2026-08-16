#ifndef DUB_RING_BUFFER_H
#define DUB_RING_BUFFER_H

#include <stddef.h>

typedef struct DubRingBuffer DubRingBuffer;

DubRingBuffer *dub_ring_create(size_t capacity_frames);
void dub_ring_destroy(DubRingBuffer *ring);
size_t dub_ring_write(DubRingBuffer *ring, const float *samples, size_t frames);
size_t dub_ring_read(DubRingBuffer *ring, float *samples, size_t max_frames);
size_t dub_ring_available(const DubRingBuffer *ring);

#endif
