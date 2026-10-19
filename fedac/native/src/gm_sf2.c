// gm_sf2.c — General MIDI from a SoundFont bank. See gm_sf2.h.

#include "gm_sf2.h"

#include <math.h>
#include <stdio.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdint.h>
#include <string.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#ifdef __clang__
#pragma clang diagnostic ignored "-Wnull-pointer-subtraction"
#endif
#define TSF_IMPLEMENTATION
#define TSF_STATIC
#include "tsf.h"
#pragma GCC diagnostic pop

#define SLOTS      48    // TSF channels: one live note each
#define MAX_VOICES 96    // stereo and layered presets spend 2+ per note
#define RING       256   // queued commands between two render calls
#define SRC_CAP    1024  // source frames rendered per TSF call

enum { CMD_ON, CMD_OFF, CMD_FADE, CMD_SET };

typedef struct {
    uint8_t op, slot;
    uint16_t gen;
    int preset, key;
    float vel, tune, pan, vol;  // tune in semitones off `key`
    int32_t frames;             // ON: auto note-off, FADE: ramp length; -1 = none
} Cmd;

static tsf *sf;
static int sf_rate = GM_SF2_RATE;
static atomic_int ready;
static char bank_name[64];

// Producer side. The mutex only orders producers among themselves; the
// audio thread reads the ring through the two atomics and never takes it.
static pthread_mutex_t push_lock = PTHREAD_MUTEX_INITIALIZER;
static struct { uint16_t gen; uint8_t held; int key; float vel; uint32_t born; } own[SLOTS];
static uint32_t born_clock;
static Cmd ring[RING];
static atomic_uint ring_w, ring_r;

// Audio side.
static struct { uint16_t gen; int key; float vol, fade, fade_step; int32_t off_in; } play[SLOTS];
static float blk[SRC_CAP * 2];
static float prev_a[2], prev_b[2];
static double frac;

int gm_sf2_ready(void) { return atomic_load_explicit(&ready, memory_order_acquire); }
const char *gm_sf2_name(void) { return bank_name; }

int gm_sf2_load(const char *path, int sample_rate) {
    if (gm_sf2_ready() || !path) return gm_sf2_ready();
    tsf *t = tsf_load_filename(path);
    if (!t) return 0;
    tsf_set_output(t, TSF_STEREO_INTERLEAVED, sample_rate, 0);
    // Preallocate voices and every channel now: TSF grows both lazily with
    // realloc, which must never happen on the audio thread.
    tsf_set_max_voices(t, MAX_VOICES);
    for (int ch = SLOTS - 1; ch >= 0; ch--) tsf_channel_set_presetindex(t, ch, 0);
    const char *base = strrchr(path, '/');
    snprintf(bank_name, sizeof bank_name, "%s", base ? base + 1 : path);
    sf = t;
    sf_rate = sample_rate;
    atomic_store_explicit(&ready, 1, memory_order_release);
    return 1;
}

static int push(const Cmd *c) {
    unsigned w = atomic_load_explicit(&ring_w, memory_order_relaxed);
    unsigned r = atomic_load_explicit(&ring_r, memory_order_acquire);
    if (w - r >= RING) return 0;
    ring[w % RING] = *c;
    atomic_store_explicit(&ring_w, w + 1, memory_order_release);
    return 1;
}

// Oldest released slot first, so a fresh note rarely lands on a tail that is
// still ringing; with every slot held, steal the oldest.
static int pick_slot(void) {
    int best = -1, best_held = 2;
    uint32_t best_age = 0;
    for (int s = 0; s < SLOTS; s++) {
        uint32_t age = born_clock - own[s].born;
        if (own[s].held < best_held || (own[s].held == best_held && age > best_age)) {
            best = s; best_held = own[s].held; best_age = age;
        }
    }
    return best;
}

static int handle_slot(int h, int *slot) {
    int s = h & 0xff;
    if (h <= 0 || s >= SLOTS || own[s].gen != (uint16_t)(h >> 8)) return 0;
    *slot = s;
    return 1;
}

int gm_sf2_note_on(int program, double midi, float velocity, float pan,
                   double duration_s, int is_drum) {
    if (!gm_sf2_ready() || !isfinite(midi)) return 0;
    int bank = is_drum ? 128 : 0;
    int preset = tsf_get_presetindex(sf, bank, program & 127);
    if (preset < 0 && is_drum) preset = tsf_get_presetindex(sf, 128, 0);
    if (preset < 0) return 0;

    int key = (int)lround(midi);
    if (key < 0) key = 0;
    if (key > 127) key = 127;
    Cmd c = { .op = CMD_ON, .preset = preset, .key = key,
              .tune = is_drum ? 0.0f : (float)(midi - key),
              .vel = velocity < 0.01f ? 0.01f : velocity > 1.0f ? 1.0f : velocity,
              .pan = pan < -1.0f ? 0.0f : pan > 1.0f ? 1.0f : (pan + 1.0f) * 0.5f,
              .frames = duration_s >= 0 && duration_s < 3600
                        ? (int32_t)(duration_s * sf_rate) : -1 };

    pthread_mutex_lock(&push_lock);
    int s = pick_slot();
    uint16_t gen = (uint16_t)(own[s].gen + 1);
    if (!gen) gen = 1;
    c.slot = (uint8_t)s;
    c.gen = gen;
    int h = 0;
    if (push(&c)) {
        own[s].gen = gen;
        own[s].key = key;
        own[s].vel = c.vel;
        own[s].held = c.frames < 0;   // timed notes free their slot themselves
        own[s].born = ++born_clock;
        h = (gen << 8) | s;
    }
    pthread_mutex_unlock(&push_lock);
    return h;
}

void gm_sf2_note_off(int handle, double fade_s) {
    if (!gm_sf2_ready()) return;
    pthread_mutex_lock(&push_lock);
    int s;
    if (handle_slot(handle, &s)) {
        Cmd c = { .op = CMD_OFF, .slot = (uint8_t)s, .gen = own[s].gen, .frames = -1 };
        if (fade_s > GM_SF2_RELEASE_MAX) {
            c.op = CMD_FADE;
            c.frames = (int32_t)(fade_s * sf_rate);
        }
        if (push(&c)) own[s].held = 0;
    }
    pthread_mutex_unlock(&push_lock);
}

void gm_sf2_update(int handle, double midi, float volume, float pan) {
    if (!gm_sf2_ready()) return;
    pthread_mutex_lock(&push_lock);
    int s;
    if (handle_slot(handle, &s)) {
        Cmd c = { .op = CMD_SET, .slot = (uint8_t)s, .gen = own[s].gen,
                  .tune = midi >= 0 ? (float)(midi - own[s].key) : NAN,
                  .vol = volume >= 0 ? volume / own[s].vel : -1.0f, .pan = pan < -1.0f ? NAN : (pan > 1.0f ? 1.0f : (pan + 1.0f) * 0.5f) };
        push(&c);
    }
    pthread_mutex_unlock(&push_lock);
}

static void apply(const Cmd *c) {
    int ch = c->slot;
    if (c->op == CMD_ON) {
        // The slot's previous tenant may still be in its release tail.
        tsf_channel_sounds_off_all(sf, ch);
        tsf_channel_set_presetindex(sf, ch, c->preset);
        tsf_channel_set_tuning(sf, ch, c->tune);
        tsf_channel_set_pan(sf, ch, c->pan);
        tsf_channel_set_volume(sf, ch, 1.0f);
        tsf_channel_note_on(sf, ch, c->key, c->vel);
        play[ch].gen = c->gen;
        play[ch].key = c->key;
        play[ch].vol = play[ch].fade = 1.0f;
        play[ch].fade_step = 0.0f;
        play[ch].off_in = c->frames;
        return;
    }
    if (play[ch].gen != c->gen) return;
    if (c->op == CMD_OFF) {
        tsf_channel_note_off(sf, ch, play[ch].key);
        play[ch].off_in = -1;
    } else if (c->op == CMD_FADE) {
        play[ch].fade_step = 1.0f / (float)(c->frames > 0 ? c->frames : 1);
        play[ch].off_in = -1;
    } else {
        if (!isnan(c->tune)) tsf_channel_set_tuning(sf, ch, c->tune);
        if (!isnan(c->pan)) tsf_channel_set_pan(sf, ch, c->pan);
        if (c->vol >= 0.0f) {
            play[ch].vol = c->vol > 2.0f ? 2.0f : c->vol;
            tsf_channel_set_volume(sf, ch, play[ch].vol * play[ch].fade);
        }
    }
}

// Timed note-offs and linger ramps, at render-call granularity (~1 ms).
static void tick(int n) {
    for (int ch = 0; ch < SLOTS; ch++) {
        if (play[ch].off_in >= 0 && (play[ch].off_in -= n) < 0) {
            tsf_channel_note_off(sf, ch, play[ch].key);
            play[ch].off_in = -1;
        }
        if (play[ch].fade_step > 0.0f) {
            play[ch].fade -= play[ch].fade_step * n;
            if (play[ch].fade <= 0.0f) {
                play[ch].fade = 0.0f;
                play[ch].fade_step = 0.0f;
                tsf_channel_sounds_off_all(sf, ch);
            }
            tsf_channel_set_volume(sf, ch, play[ch].vol * play[ch].fade);
        }
    }
}

void gm_sf2_render(float *out, int frames, int out_rate) {
    if (!gm_sf2_ready() || out_rate <= 0) {
        memset(out, 0, sizeof(float) * 2 * (size_t)frames);
        return;
    }
    unsigned r = atomic_load_explicit(&ring_r, memory_order_relaxed);
    unsigned w = atomic_load_explicit(&ring_w, memory_order_acquire);
    for (; r != w; r++) apply(&ring[r % RING]);
    atomic_store_explicit(&ring_r, r, memory_order_release);

    // Linear interpolation between source frames a and b; each carry pulls
    // the next rendered frame. The pre-pass counts carries with the same
    // arithmetic, so exactly that many frames are rendered and none dropped.
    const double step = (double)sf_rate / out_rate;
    const int max_sub = (int)((SRC_CAP - 2) / step);
    for (int i = 0; i < frames;) {
        int sub = frames - i < max_sub ? frames - i : max_sub;
        double f = frac;
        int need = 0;
        for (int j = 0; j < sub; j++)
            for (f += step; f >= 1.0; f -= 1.0) need++;
        if (need > 0) {
            tick(need);
            tsf_render_float(sf, blk, need, 0);
        }
        int k = 0;
        for (int j = 0; j < sub; j++, i++) {
            float t = (float)frac;
            out[2 * i]     = prev_a[0] + (prev_b[0] - prev_a[0]) * t;
            out[2 * i + 1] = prev_a[1] + (prev_b[1] - prev_a[1]) * t;
            for (frac += step; frac >= 1.0; frac -= 1.0, k++) {
                prev_a[0] = prev_b[0]; prev_a[1] = prev_b[1];
                prev_b[0] = blk[2 * k]; prev_b[1] = blk[2 * k + 1];
            }
        }
    }
}
