#include <AudioToolbox/AudioToolbox.h>
#include <CoreFoundation/CoreFoundation.h>
#include <math.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define SR 48000
#define CHANNELS 2
#define QUEUE 512
#define MAX_NOTES 24
#define MAX_CHORD 6
#define MAX_VOICES 48
#define TAU 6.2831853071795864769

typedef struct { uint8_t slot, midi, duration, velocity; } Note;
typedef struct {
  uint64_t bar;
  float bpm;
  uint8_t chord_count, note_count;
  uint8_t chord[MAX_CHORD];
  Note notes[MAX_NOTES];
  uint16_t pulse_mask;
  uint64_t context_hash;
} MeasurePlan;
typedef struct { MeasurePlan plan; _Atomic uint64_t ready; } PlanSlot;
typedef enum { PAD, LEAD, BASS, PULSE } VoiceKind;
typedef struct {
  int active;
  VoiceKind kind;
  double phase, freq, amp;
  uint64_t age, length;
} Voice;
typedef struct {
  PlanSlot plans[QUEUE];
  MeasurePlan current;
  Voice voices[MAX_VOICES];
  uint64_t sample_in_bar, samples_in_bar;
  int last_slot;
  _Atomic uint64_t audible_bar;
  _Atomic uint32_t audible_bpm_x100;
  _Atomic uint64_t audible_context_hash;
  _Atomic uint64_t fallback_count;
  _Atomic int running;
} Engine;

static Engine engine;

static double midi_hz(int midi) { return 440.0 * pow(2.0, (midi - 69) / 12.0); }

static void voice_start(VoiceKind kind, int midi, double amp, uint64_t length) {
  for (int i = 0; i < MAX_VOICES; i++) if (!engine.voices[i].active) {
    engine.voices[i] = (Voice){1, kind, 0.0, midi_hz(midi), amp, 0, length};
    return;
  }
}

static MeasurePlan fallback_plan(uint64_t bar) {
  MeasurePlan p = engine.current;
  if (p.bpm < 40) {
    p.bpm = 112;
    p.chord_count = 4;
    uint8_t chord[4] = {52, 55, 59, 62};
    memcpy(p.chord, chord, 4);
    p.note_count = 4;
    Note notes[4] = {{0,64,3,92},{4,67,3,82},{8,71,4,96},{12,69,3,78}};
    memcpy(p.notes, notes, sizeof(notes));
    p.pulse_mask = 0x8888;
  } else {
    int shift = (bar % 4 == 3) ? 2 : 0;
    for (int i = 0; i < p.note_count; i++) p.notes[i].midi = (uint8_t)(p.notes[i].midi + shift);
  }
  p.bar = bar;
  return p;
}

static void load_bar(uint64_t bar) {
  PlanSlot *slot = &engine.plans[bar % QUEUE];
  uint64_t wanted = bar + 1;
  if (atomic_load_explicit(&slot->ready, memory_order_acquire) == wanted) {
    engine.current = slot->plan;
    atomic_store_explicit(&slot->ready, 0, memory_order_release);
  } else {
    engine.current = fallback_plan(bar);
    atomic_fetch_add(&engine.fallback_count, 1);
  }
  engine.current.bar = bar;
  engine.samples_in_bar = (uint64_t)llround(SR * 240.0 / engine.current.bpm);
  engine.sample_in_bar = 0;
  engine.last_slot = -1;
  atomic_store_explicit(&engine.audible_bpm_x100,
    (uint32_t)llround(engine.current.bpm * 100.0), memory_order_release);
  atomic_store_explicit(&engine.audible_context_hash,
    engine.current.context_hash, memory_order_release);
  atomic_store_explicit(&engine.audible_bar, bar, memory_order_release);
  for (int i = 0; i < engine.current.chord_count; i++)
    voice_start(PAD, engine.current.chord[i], 0.035 / sqrt(engine.current.chord_count), engine.samples_in_bar);
  if (engine.current.chord_count) voice_start(BASS, engine.current.chord[0] - 12, 0.075, engine.samples_in_bar);
}

static void trigger_slot(int slot) {
  if ((engine.current.pulse_mask >> (15 - slot)) & 1)
    voice_start(PULSE, 31, slot % 4 == 0 ? 0.12 : 0.055, SR / 7);
  for (int i = 0; i < engine.current.note_count; i++) {
    Note *n = &engine.current.notes[i];
    if (n->slot == slot) {
      uint64_t length = (uint64_t)((double)engine.samples_in_bar * n->duration / 16.0);
      voice_start(LEAD, n->midi, 0.14 * n->velocity / 127.0, length);
    }
  }
}

static double voice_sample(Voice *v) {
  if (!v->active) return 0;
  double t = (double)v->age / SR;
  double remain = (double)(v->length - v->age) / SR;
  double attack = fmin(1.0, t / (v->kind == PAD ? 0.08 : 0.008));
  double release = fmin(1.0, remain / (v->kind == PAD ? 0.18 : 0.08));
  double env = attack * release;
  double s;
  if (v->kind == PULSE) {
    double f = v->freq * exp(-t * 8.0);
    v->phase += f / SR;
    s = sin(TAU * v->phase) * exp(-t * 18.0);
  } else {
    v->phase += v->freq / SR;
    s = sin(TAU * v->phase);
    if (v->kind == PAD) s = 0.78 * s + 0.22 * sin(TAU * v->phase * 2.001);
    if (v->kind == LEAD) env *= exp(-t / 0.65);
  }
  v->age++;
  if (v->age >= v->length) v->active = 0;
  return s * env * v->amp;
}

static void render(float *out, uint32_t frames) {
  for (uint32_t frame = 0; frame < frames; frame++) {
    if (!engine.samples_in_bar || engine.sample_in_bar >= engine.samples_in_bar)
      load_bar(engine.samples_in_bar ? engine.current.bar + 1 : 0);
    int slot = (int)((engine.sample_in_bar * 16) / engine.samples_in_bar);
    if (slot != engine.last_slot) { trigger_slot(slot); engine.last_slot = slot; }
    double sample = 0;
    for (int i = 0; i < MAX_VOICES; i++) sample += voice_sample(&engine.voices[i]);
    sample = tanh(sample * 1.4) * 0.72;
    out[frame * 2] = (float)sample;
    out[frame * 2 + 1] = (float)sample;
    engine.sample_in_bar++;
  }
}

static void audio_callback(void *data, AudioQueueRef queue, AudioQueueBufferRef buffer) {
  (void)data;
  uint32_t frames = buffer->mAudioDataBytesCapacity / (sizeof(float) * CHANNELS);
  render((float *)buffer->mAudioData, frames);
  buffer->mAudioDataByteSize = frames * sizeof(float) * CHANNELS;
  AudioQueueEnqueueBuffer(queue, buffer, 0, NULL);
}

static int parse_plan(char *line, MeasurePlan *p) {
  char *save = NULL;
  char *tag = strtok_r(line, "|", &save);
  char *bar = strtok_r(NULL, "|", &save), *bpm = strtok_r(NULL, "|", &save);
  char *chords = strtok_r(NULL, "|", &save), *notes = strtok_r(NULL, "|", &save);
  char *mask = strtok_r(NULL, "|", &save), *hash = strtok_r(NULL, "|\n", &save);
  if (!tag || strcmp(tag, "P") || !bar || !bpm || !chords || !notes || !mask) return 0;
  memset(p, 0, sizeof(*p));
  p->bar = strtoull(bar, NULL, 10); p->bpm = strtof(bpm, NULL);
  char *c_save = NULL;
  for (char *c = strtok_r(chords, ",", &c_save); c && p->chord_count < MAX_CHORD; c = strtok_r(NULL, ",", &c_save))
    p->chord[p->chord_count++] = (uint8_t)atoi(c);
  char *n_save = NULL;
  for (char *entry = strtok_r(notes, ";", &n_save); entry && p->note_count < MAX_NOTES; entry = strtok_r(NULL, ";", &n_save)) {
    Note *n = &p->notes[p->note_count];
    if (sscanf(entry, "%hhu,%hhu,%hhu,%hhu", &n->slot, &n->midi, &n->duration, &n->velocity) == 4) p->note_count++;
  }
  p->pulse_mask = (uint16_t)strtoul(mask, NULL, 16);
  p->context_hash = hash ? strtoull(hash, NULL, 16) : 0;
  return p->bpm >= 40 && p->bpm <= 240;
}

static void *reader(void *unused) {
  (void)unused;
  char line[2048];
  while (atomic_load(&engine.running) && fgets(line, sizeof(line), stdin)) {
    MeasurePlan p;
    if (!parse_plan(line, &p)) continue;
    PlanSlot *slot = &engine.plans[p.bar % QUEUE];
    slot->plan = p;
    atomic_store_explicit(&slot->ready, p.bar + 1, memory_order_release);
  }
  return NULL;
}

static void stop_engine(int signal) { (void)signal; atomic_store(&engine.running, 0); }

static void wav_header(FILE *file, uint32_t data_bytes) {
  uint32_t riff_bytes = 36 + data_bytes, rate = SR, byte_rate = SR * CHANNELS * sizeof(float);
  uint16_t format = 3, channels = CHANNELS, block = CHANNELS * sizeof(float), bits = 32;
  fwrite("RIFF", 1, 4, file); fwrite(&riff_bytes, 4, 1, file); fwrite("WAVEfmt ", 1, 8, file);
  uint32_t fmt_bytes = 16;
  fwrite(&fmt_bytes, 4, 1, file); fwrite(&format, 2, 1, file); fwrite(&channels, 2, 1, file);
  fwrite(&rate, 4, 1, file); fwrite(&byte_rate, 4, 1, file); fwrite(&block, 2, 1, file);
  fwrite(&bits, 2, 1, file); fwrite("data", 1, 4, file); fwrite(&data_bytes, 4, 1, file);
}

static int render_wav(const char *path, uint64_t bars) {
  char line[2048];
  while (fgets(line, sizeof(line), stdin)) {
    MeasurePlan p;
    if (!parse_plan(line, &p) || p.bar >= QUEUE) continue;
    PlanSlot *slot = &engine.plans[p.bar % QUEUE];
    slot->plan = p;
    atomic_store_explicit(&slot->ready, p.bar + 1, memory_order_release);
  }
  FILE *file = fopen(path, "wb+");
  if (!file) { perror(path); return 1; }
  wav_header(file, 0);
  float buffer[512 * CHANNELS];
  uint64_t frames_written = 0;
  load_bar(0);
  for (;;) {
    uint64_t remaining = engine.samples_in_bar - engine.sample_in_bar;
    uint32_t frames = remaining < 512 ? (uint32_t)remaining : 512;
    render(buffer, frames);
    fwrite(buffer, sizeof(float) * CHANNELS, frames, file);
    frames_written += frames;
    if (engine.sample_in_bar >= engine.samples_in_bar) {
      if (engine.current.bar + 1 >= bars) break;
      load_bar(engine.current.bar + 1);
    }
  }
  uint64_t bytes64 = frames_written * CHANNELS * sizeof(float);
  if (bytes64 > UINT32_MAX) { fprintf(stderr, "render exceeds WAV32 limit\n"); fclose(file); return 1; }
  rewind(file); wav_header(file, (uint32_t)bytes64); fclose(file);
  fprintf(stderr, "RENDER bars=%llu seconds=%.3f fallbacks=%llu path=%s\n",
    (unsigned long long)bars, (double)frames_written / SR,
    (unsigned long long)atomic_load(&engine.fallback_count), path);
  return atomic_load(&engine.fallback_count) ? 2 : 0;
}

int main(int argc, char **argv) {
  int seconds = 120;
  uint64_t render_bars = 32;
  const char *render_path = NULL;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--seconds") && i + 1 < argc) seconds = atoi(argv[++i]);
    else if (!strcmp(argv[i], "--bars") && i + 1 < argc) render_bars = strtoull(argv[++i], NULL, 10);
    else if (!strcmp(argv[i], "--render") && i + 1 < argc) render_path = argv[++i];
  }
  memset(&engine, 0, sizeof(engine));
  atomic_store(&engine.running, 1);
  atomic_store(&engine.audible_bar, UINT64_MAX);
  if (render_path) return render_wav(render_path, render_bars);
  signal(SIGINT, stop_engine); signal(SIGTERM, stop_engine);
  pthread_t input_thread;
  pthread_create(&input_thread, NULL, reader, NULL);

  AudioStreamBasicDescription format = {0};
  format.mSampleRate = SR; format.mFormatID = kAudioFormatLinearPCM;
  format.mFormatFlags = kLinearPCMFormatFlagIsFloat | kAudioFormatFlagIsPacked;
  format.mBytesPerPacket = sizeof(float) * CHANNELS; format.mFramesPerPacket = 1;
  format.mBytesPerFrame = sizeof(float) * CHANNELS; format.mChannelsPerFrame = CHANNELS;
  format.mBitsPerChannel = 32;
  AudioQueueRef queue;
  OSStatus status = AudioQueueNewOutput(&format, audio_callback, NULL, NULL, NULL, 0, &queue);
  if (status) { fprintf(stderr, "AudioQueueNewOutput failed: %d\n", (int)status); return 1; }
  for (int i = 0; i < 3; i++) {
    AudioQueueBufferRef buffer;
    AudioQueueAllocateBuffer(queue, 512 * sizeof(float) * CHANNELS, &buffer);
    audio_callback(NULL, queue, buffer);
  }
  AudioQueueStart(queue, NULL);
  fprintf(stderr, "READY sample_rate=%d callback_frames=512 lookahead=external\n", SR);
  uint64_t seen = UINT64_MAX;
  for (int tick = 0; atomic_load(&engine.running) && (seconds <= 0 || tick < seconds * 100); tick++) {
    uint64_t bar = atomic_load_explicit(&engine.audible_bar, memory_order_acquire);
    if (bar != UINT64_MAX && bar != seen) {
      seen = bar;
      uint32_t bpm_x100 = atomic_load_explicit(&engine.audible_bpm_x100, memory_order_acquire);
      uint64_t context_hash = atomic_load_explicit(&engine.audible_context_hash, memory_order_acquire);
      fprintf(stderr, "BAR %llu bpm=%.2f context=%llx fallbacks=%llu\n",
        (unsigned long long)bar, bpm_x100 / 100.0,
        (unsigned long long)context_hash,
        (unsigned long long)atomic_load(&engine.fallback_count));
      fflush(stderr);
    }
    usleep(10000);
  }
  atomic_store(&engine.running, 0);
  AudioQueueStop(queue, true); AudioQueueDispose(queue, true);
  return 0;
}
