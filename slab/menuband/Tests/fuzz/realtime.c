// Coverage-guided stateful checks for the control/render C boundaries.
// No audio device, UI, network, preferences, or live app is involved.
#include "MBRealtimeQueue.h"
#include <assert.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

static void collect(MBLatencyProbe *probe, uint64_t *delivered) {
    MBLatencySample sample;
    while (mb_latency_pop(probe, &sample)) {
        assert(sample.event > 0 && sample.event <= sample.captured);
        assert(sample.captured <= sample.queued && sample.queued <= sample.staged);
        assert(sample.staged <= sample.rendered && isfinite(sample.rendered));
        ++*delivered;
    }
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    MBRealtimeQueue *queue = mb_realtime_queue_create();
    MBLatencyProbe *probe = mb_latency_create();
    assert(queue && probe);
    const unsigned capacity = mb_realtime_queue_capacity();
    assert(capacity == 256);
    uint32_t expected[256], sequence = 0;
    unsigned head = 0, count = 0;
    bool resetting = false;
    uint64_t staged = 0, delivered = 0;
    for (size_t i = 0; i < size; ++i) {
        uint8_t byte = data[i];
        switch (byte % 12) {
        case 0: case 1: case 2: {
            MBRealtimeCommand c = {.kind = (byte % 3) + 1, .seed = ++sequence,
                .program = byte, .note = byte, .channel = byte, .velocity = byte};
            bool accepted = mb_realtime_queue_push(queue, c);
            bool wanted = !resetting && count < capacity;
            assert(accepted == wanted);
            if (wanted) { expected[(head + count) % capacity] = sequence; ++count; }
            else { resetting = true; }
            break;
        }
        case 3: {
            MBRealtimeCommand c;
            bool found = mb_realtime_queue_pop(queue, &c);
            assert(found == (count != 0));
            if (found) { assert(c.seed == expected[head]); head = (head + 1) % capacity; --count; }
            break;
        }
        case 4:
            assert(mb_realtime_queue_take_reset(queue) == resetting);
            if (resetting) { count = 0; head = 0; resetting = false; }
            break;
        case 5: case 6: {
            uint64_t bits = (uint64_t)byte * UINT64_C(0x0101010101010101);
            double pitch, loaded;
            memcpy(&pitch, &bits, sizeof(pitch));
            mb_realtime_queue_set_pitch(queue, pitch);
            loaded = mb_realtime_queue_pitch(queue);
            uint64_t result;
            memcpy(&result, &loaded, sizeof(result));
            assert(result == bits);
            break;
        }
        case 7: case 8: {
            double t = (double)i + 1;
            MBLatencySample sample = {.source = byte % 3 + 1, .event = t,
                .captured = t + .001, .queued = t + .002,
                .staged = (byte & 0x80) ? 0 : t + .003};
            (void)mb_latency_stage(probe, sample);
            ++staged;
            break;
        }
        case 9: mb_latency_begin_render(probe); break;
        case 10: mb_latency_end_render(probe, (double)i + 2, (byte & 0x80) == 0); break;
        case 11: collect(probe, &delivered); break;
        }
    }
    mb_latency_end_render(probe, (double)size + 2, true);
    collect(probe, &delivered);
    mb_latency_begin_render(probe);
    mb_latency_end_render(probe, (double)size + 2, true);
    collect(probe, &delivered);
    assert(delivered + mb_latency_take_drops(probe) == staged);
    mb_latency_destroy(probe);
    mb_realtime_queue_destroy(queue);
    return 0;
}

#ifdef MB_STANDALONE_FUZZ
// Seeded mutation fallback for Xcode installations without libFuzzer's runtime.
#include <stdio.h>
#include <stdlib.h>
static uint64_t random_state;
static uint64_t next_random(void) {
    random_state ^= random_state << 13;
    random_state ^= random_state >> 7;
    random_state ^= random_state << 17;
    return random_state;
}
int main(int argc, char **argv) {
    unsigned runs = argc > 1 ? (unsigned)strtoul(argv[1], NULL, 10) : 50000;
    random_state = argc > 2 ? strtoull(argv[2], NULL, 0) : UINT64_C(0x4d4246555a5a);
    if (!random_state) random_state = 1;
    fprintf(stderr, "seed=%llu runs=%u\n", (unsigned long long)random_state, runs);
    uint8_t input[4096];
    size_t length = 1024;
    memset(input, 0, sizeof(input));
    // Explicitly reach both rings' overflow and error-reset boundaries.
    for (unsigned op = 0; op < 12; ++op) {
        memset(input, op, 1024);
        input[1024] = 9; input[1025] = 10; input[1026] = 11;
        LLVMFuzzerTestOneInput(input, 1027);
    }
    for (unsigned run = 0; run < runs; ++run) {
        if (run % 128 == 0) {
            length = 1 + next_random() % sizeof(input);
            for (size_t i = 0; i < length; ++i) input[i] = (uint8_t)next_random();
        }
        switch (next_random() % 4) {
        case 0:
            length = 1 + next_random() % sizeof(input);
            break;
        case 1: {
            size_t start = next_random() % length;
            size_t span = next_random() % (length - start + 1);
            memset(input + start, (uint8_t)next_random(), span);
            break;
        }
        default:
            for (unsigned m = 0, n = 1 + next_random() % 32; m < n; ++m)
                input[next_random() % length] = (uint8_t)next_random();
        }
        LLVMFuzzerTestOneInput(input, length);
    }
    fprintf(stderr, "PASS: %u mutated sequences plus 12 boundary seeds\n", runs);
    return 0;
}
#endif
