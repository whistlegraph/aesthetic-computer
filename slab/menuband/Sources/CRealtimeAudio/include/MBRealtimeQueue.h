#pragma once
#include <stdbool.h>
#include <stdint.h>

// One consumer (audio render), producers serialized by the control-side lock.
// Allocation happens only at create/destroy. No consumer locks or retries.
typedef struct MBRealtimeQueue MBRealtimeQueue;
typedef struct {
    uint32_t kind;
    uint32_t seed;
    int32_t program;
    uint8_t note, channel, velocity;
} MBRealtimeCommand;

MBRealtimeQueue *mb_realtime_queue_create(void);
void mb_realtime_queue_destroy(MBRealtimeQueue *queue);
bool mb_realtime_queue_push(MBRealtimeQueue *queue, MBRealtimeCommand command);
bool mb_realtime_queue_pop(MBRealtimeQueue *queue, MBRealtimeCommand *command);
// On overflow discard the old backlog and silence voices. New events are
// refused until the consumer acknowledges this reset: releases cannot be lost
// while a corresponding onset survives.
bool mb_realtime_queue_take_reset(MBRealtimeQueue *queue);
void mb_realtime_queue_set_pitch(MBRealtimeQueue *queue, double pitch);
double mb_realtime_queue_pitch(MBRealtimeQueue *queue);
uint32_t mb_realtime_queue_capacity(void);

// Opt-in timing only: no keys, MIDI notes or audio. Control -> render -> logger.
typedef struct MBLatencyProbe MBLatencyProbe;
typedef struct {
    uint32_t source;
    double event, captured, queued, staged, rendered;
} MBLatencySample;
MBLatencyProbe *mb_latency_create(void);
void mb_latency_destroy(MBLatencyProbe *probe);
bool mb_latency_stage(MBLatencyProbe *probe, MBLatencySample sample);
void mb_latency_begin_render(MBLatencyProbe *probe);
void mb_latency_end_render(MBLatencyProbe *probe, double now, bool valid);
bool mb_latency_pop(MBLatencyProbe *probe, MBLatencySample *sample);
uint64_t mb_latency_take_drops(MBLatencyProbe *probe);
