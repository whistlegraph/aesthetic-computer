// synth_core.c — Platform-agnostic synth engine extracted from audio.c.
// All oscillators, gun presets, envelope math, and voice lifecycle helpers
// live here; platform drivers (ALSA on Linux, SDL3 on macOS) own device
// I/O, threading, effects chains, sample playback, and mixing decisions.
//
// Implementation closely mirrors fedac/native/src/audio.c sections for the
// matching features — same constants, same biquad topology. Keep in sync
// with that file until the Linux driver finishes migrating to this core.

#include "synth_core.h"

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// ─── Note table / freq parser ───────────────────────────────────────────────

static const struct { const char *name; double freq; } note_table[] = {
    {"c",  16.3516}, {"cs", 17.3239}, {"db", 17.3239},
    {"d",  18.3540}, {"ds", 19.4454}, {"eb", 19.4454},
    {"e",  20.6017}, {"f",  21.8268}, {"fs", 23.1247},
    {"gb", 23.1247}, {"g",  24.4997}, {"gs", 25.9565},
    {"ab", 25.9565}, {"a",  27.5000}, {"as", 29.1352},
    {"bb", 29.1352}, {"b",  30.8677},
};
#define NOTE_TABLE_SIZE (sizeof(note_table) / sizeof(note_table[0]))

double synth_note_to_freq(const char *note) {
    if (!note || !*note) return 440.0;
    char *end;
    double d = strtod(note, &end);
    if (end != note && *end == '\0') return d;
    int octave = 4;
    char name_buf[8] = {0};
    int ni = 0;
    const char *p = note;
    if (*p >= '0' && *p <= '9') { octave = *p - '0'; p++; }
    while (*p && ni < 3) {
        char ch = *p;
        if (ch >= 'A' && ch <= 'G') ch += 32;
        if ((ch >= 'a' && ch <= 'g') || ch == '#' || ch == 's' || ch == 'b') {
            if (ch == '#') name_buf[ni++] = 's';
            else           name_buf[ni++] = ch;
            p++;
        } else break;
    }
    name_buf[ni] = '\0';
    if (*p >= '0' && *p <= '9') octave = *p - '0';
    double base = 440.0;
    for (int i = 0; i < (int)NOTE_TABLE_SIZE; i++) {
        if (strcmp(name_buf, note_table[i].name) == 0) {
            base = note_table[i].freq;
            break;
        }
    }
    return base * pow(2.0, octave);
}

WaveType synth_parse_wave(const char *s) {
    if (!s) return WAVE_SINE;
    if (!strcmp(s, "sine"))     return WAVE_SINE;
    if (!strcmp(s, "triangle")) return WAVE_TRIANGLE;
    if (!strcmp(s, "sawtooth")) return WAVE_SAWTOOTH;
    if (!strcmp(s, "saw"))      return WAVE_SAWTOOTH;
    if (!strcmp(s, "square"))   return WAVE_SQUARE;
    if (!strcmp(s, "noise"))    return WAVE_NOISE;
    if (!strcmp(s, "whistle"))  return WAVE_WHISTLE;
    if (!strcmp(s, "gun"))      return WAVE_GUN;
    return WAVE_SINE;
}

// ─── Helpers ────────────────────────────────────────────────────────────────

static inline uint32_t xorshift32(uint32_t *state) {
    uint32_t x = *state;
    x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    *state = x;
    return x;
}

static inline double clampd(double x, double lo, double hi) {
    return x < lo ? lo : (x > hi ? hi : x);
}

double synth_compute_envelope(const ACVoice *v) {
    double env = 1.0;
    if (v->attack > 0.0 && v->elapsed < v->attack) {
        env = v->elapsed / v->attack;
    }
    if (!isinf(v->duration) && v->decay > 0.0) {
        double decay_start = v->duration - v->decay;
        if (decay_start < 0.0) decay_start = 0.0;
        if (v->elapsed > decay_start) {
            double p = (v->elapsed - decay_start) / v->decay;
            if (p > 1.0) p = 1.0;
            env *= (1.0 - p);
        }
    }
    return env;
}

double synth_compute_fade(const ACVoice *v) {
    if (v->state != VOICE_KILLING) return 1.0;
    if (v->fade_duration <= 0.0) return 0.0;
    double progress = v->fade_elapsed / v->fade_duration;
    if (progress >= 1.0) return 0.0;
    return 1.0 - progress;
}

// Fractional-delay ring read for whistle/gun-physical bore loops.
static inline double whistle_frac_read(const float *buf, int N, int w, double delay) {
    if (delay < 0.0) delay = 0.0;
    if (delay > (double)(N - 2)) delay = (double)(N - 2);
    double rd = (double)w - delay;
    while (rd < 0.0) rd += (double)N;
    int i0 = (int)rd;
    int i1 = (i0 + 1) % N;
    double f = rd - (double)i0;
    return (double)buf[i0] * (1.0 - f) + (double)buf[i1] * f;
}

// ─── Whistle (STK flute DWG) ────────────────────────────────────────────────

static inline double generate_whistle_sample(ACVoice *v, double sample_rate) {
    double env = synth_compute_envelope(v);
    double breath_target = 0.18 + 0.82 * sqrt(env);
    double breath_slew = env > v->whistle_breath ? 0.012 : 0.003;
    v->whistle_breath += (breath_target - v->whistle_breath) * breath_slew;

    v->whistle_vibrato_phase += 5.0 / sample_rate;
    if (v->whistle_vibrato_phase >= 1.0) v->whistle_vibrato_phase -= 1.0;
    double vibrato = sin(2.0 * M_PI * v->whistle_vibrato_phase) * 0.03;

    double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
    double onset = 1.0 - env;
    double noise_gain = 0.08 + 0.05 * onset;
    double breath = v->whistle_breath * (1.0 + noise_gain * white + vibrato);

    double freq = clampd(v->frequency, 110.0, sample_rate * 0.20);
    double bore_delay = sample_rate / freq;
    double jet_delay = bore_delay * 0.32;
    const int BORE_N = 2048;
    const int JET_N = 512;
    if (bore_delay > (double)(BORE_N - 2)) bore_delay = (double)(BORE_N - 2);
    if (jet_delay > (double)(JET_N - 2)) jet_delay = (double)(JET_N - 2);

    double bore_out = whistle_frac_read(v->whistle_bore_buf, BORE_N, v->whistle_bore_w, bore_delay);
    v->whistle_lp1 = 0.35 * (-bore_out) + 0.65 * v->whistle_lp1;
    double temp = v->whistle_lp1;

    double jet_refl = 0.5;
    double end_refl = 0.5;
    double pd = breath - jet_refl * temp;
    v->whistle_jet_buf[v->whistle_jet_w] = (float)pd;
    v->whistle_jet_w = (v->whistle_jet_w + 1) % JET_N;
    pd = whistle_frac_read(v->whistle_jet_buf, JET_N, v->whistle_jet_w, jet_delay);

    pd = pd * (pd * pd - 1.0);
    if (pd > 1.0) pd = 1.0;
    if (pd < -1.0) pd = -1.0;
    double y = pd - v->whistle_hp_x1 + 0.995 * v->whistle_hp_y1;
    v->whistle_hp_x1 = pd;
    v->whistle_hp_y1 = y;

    double into_bore = y + end_refl * temp;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)into_bore;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % BORE_N;
    return 0.3 * into_bore;
}

// ─── Gun presets + init ─────────────────────────────────────────────────────

typedef struct {
    GunModel model;
    double master_amp;
    double secondary_delay_ms;
    double secondary_amp;
    int    sustain_fire;
    double retrig_period_ms;
    double click_amp;
    double click_decay_ms;
    double crack_amp;
    double crack_decay_ms;
    double crack_fc;
    double crack_q;
    double boom_amp;
    double boom_freq_start;
    double boom_freq_end;
    double boom_pitch_decay_ms;
    double boom_amp_decay_ms;
    double tail_amp;
    double tail_attack_ms;
    double tail_decay_ms;
    double tail_fc;
    double tail_q;
    double bore_length_s;
    double bore_loss;
    double breech_reflect;
    double pressure;
    double env_rate;
    double noise_gain;
    double body_freq[3];
    double body_q[3];
    double body_amp[3];
    double radiation;
} GunPresetParams;

// Presets ported verbatim from src/audio.c gun_presets[] — keep in sync.
static const GunPresetParams gun_presets[GUN_PRESET_COUNT] = {
    [GUN_PISTOL] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 1.1,
        .click_amp = 0.65, .click_decay_ms = 0.5,
        .crack_amp = 0.95, .crack_decay_ms = 7.0,  .crack_fc = 3800, .crack_q = 2.6,
        .boom_amp  = 0.55, .boom_freq_start = 220, .boom_freq_end = 55,
        .boom_pitch_decay_ms = 14, .boom_amp_decay_ms = 55,
        .tail_amp  = 0.35, .tail_attack_ms = 0,    .tail_decay_ms = 110,
        .tail_fc   = 900,  .tail_q = 0.8,
        .bore_length_s = 0.000588, .bore_loss = 0.55, .breech_reflect = 0.92,
        .pressure = 1.2, .env_rate = 3000.0, .noise_gain = 0.6,
        .body_freq = {1500, 4000, 8500}, .body_q = {12, 10, 8},
        .body_amp = {0.30, 0.20, 0.15}, .radiation = 0.985 },
    [GUN_RIFLE] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 1.2,
        .click_amp = 0.75, .click_decay_ms = 0.6,
        .crack_amp = 1.05, .crack_decay_ms = 8.0,  .crack_fc = 4500, .crack_q = 3.0,
        .boom_amp  = 0.70, .boom_freq_start = 280, .boom_freq_end = 50,
        .boom_pitch_decay_ms = 18, .boom_amp_decay_ms = 90,
        .tail_amp  = 0.45, .tail_attack_ms = 0,    .tail_decay_ms = 220,
        .tail_fc   = 1100, .tail_q = 0.7,
        .secondary_delay_ms = 0.9, .secondary_amp = 0.55,
        .bore_length_s = 0.00235, .bore_loss = 0.50, .breech_reflect = 0.95,
        .pressure = 1.5, .env_rate = 2500.0, .noise_gain = 0.5,
        .body_freq = {800, 2400, 6000}, .body_q = {14, 12, 10},
        .body_amp = {0.35, 0.25, 0.15}, .radiation = 0.988 },
    [GUN_SHOTGUN] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 1.4,
        .click_amp = 0.55, .click_decay_ms = 0.8,
        .crack_amp = 0.65, .crack_decay_ms = 12,   .crack_fc = 2200, .crack_q = 1.8,
        .boom_amp  = 1.10, .boom_freq_start = 260, .boom_freq_end = 38,
        .boom_pitch_decay_ms = 22, .boom_amp_decay_ms = 130,
        .tail_amp  = 0.85, .tail_attack_ms = 4,    .tail_decay_ms = 380,
        .tail_fc   = 700,  .tail_q = 0.6,
        .bore_length_s = 0.00388, .bore_loss = 0.40, .breech_reflect = 0.88,
        .pressure = 1.8, .env_rate = 1800.0, .noise_gain = 0.9,
        .body_freq = {400, 1200, 3500}, .body_q = {10, 8, 7},
        .body_amp = {0.40, 0.25, 0.15}, .radiation = 0.965 },
    [GUN_SMG] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 0.95,
        .click_amp = 0.55, .click_decay_ms = 0.4,
        .crack_amp = 0.85, .crack_decay_ms = 5.0,  .crack_fc = 4200, .crack_q = 2.5,
        .boom_amp  = 0.40, .boom_freq_start = 200, .boom_freq_end = 60,
        .boom_pitch_decay_ms = 10, .boom_amp_decay_ms = 40,
        .tail_amp  = 0.28, .tail_attack_ms = 0,    .tail_decay_ms = 80,
        .tail_fc   = 1200, .tail_q = 0.7,
        .sustain_fire = 1, .retrig_period_ms = 60,
        .bore_length_s = 0.00132, .bore_loss = 0.58, .breech_reflect = 0.92,
        .pressure = 1.0, .env_rate = 3500.0, .noise_gain = 0.5,
        .body_freq = {1200, 3500, 7500}, .body_q = {12, 10, 8},
        .body_amp = {0.30, 0.20, 0.13}, .radiation = 0.978 },
    [GUN_SUPPRESSED] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 0.7,
        .click_amp = 0.08, .click_decay_ms = 0.4,
        .crack_amp = 0.30, .crack_decay_ms = 6.0,  .crack_fc = 1600, .crack_q = 1.1,
        .boom_amp  = 0.10, .boom_freq_start = 150, .boom_freq_end = 80,
        .boom_pitch_decay_ms = 8,  .boom_amp_decay_ms = 30,
        .tail_amp  = 0.85, .tail_attack_ms = 6,    .tail_decay_ms = 140,
        .tail_fc   = 1800, .tail_q = 0.6,
        .bore_length_s = 0.00100, .bore_loss = 0.85, .breech_reflect = 0.80,
        .pressure = 0.5, .env_rate = 1500.0, .noise_gain = 1.0,
        .body_freq = {600, 1500, 3000}, .body_q = {6, 5, 4},
        .body_amp = {0.15, 0.10, 0.05}, .radiation = 0.85 },
    [GUN_LMG] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 0.9,
        .click_amp = 0.55, .click_decay_ms = 0.5,
        .crack_amp = 0.85, .crack_decay_ms = 7.0,  .crack_fc = 3500, .crack_q = 2.6,
        .boom_amp  = 0.65, .boom_freq_start = 250, .boom_freq_end = 48,
        .boom_pitch_decay_ms = 16, .boom_amp_decay_ms = 75,
        .tail_amp  = 0.40, .tail_attack_ms = 0,    .tail_decay_ms = 160,
        .tail_fc   = 950,  .tail_q = 0.7,
        .sustain_fire = 1, .retrig_period_ms = 100,
        .bore_length_s = 0.00329, .bore_loss = 0.48, .breech_reflect = 0.94,
        .pressure = 1.4, .env_rate = 2200.0, .noise_gain = 0.55,
        .body_freq = {600, 1800, 4500}, .body_q = {12, 10, 8},
        .body_amp = {0.35, 0.25, 0.15}, .radiation = 0.982 },
    [GUN_SNIPER] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 1.5,
        .click_amp = 0.85, .click_decay_ms = 0.7,
        .crack_amp = 1.20, .crack_decay_ms = 11,   .crack_fc = 5000, .crack_q = 3.2,
        .boom_amp  = 1.20, .boom_freq_start = 320, .boom_freq_end = 36,
        .boom_pitch_decay_ms = 28, .boom_amp_decay_ms = 180,
        .tail_amp  = 0.70, .tail_attack_ms = 3,    .tail_decay_ms = 500,
        .tail_fc   = 850,  .tail_q = 0.8,
        .secondary_delay_ms = 1.4, .secondary_amp = 0.70,
        .bore_length_s = 0.00435, .bore_loss = 0.35, .breech_reflect = 0.97,
        .pressure = 2.0, .env_rate = 1500.0, .noise_gain = 0.7,
        .body_freq = {350, 950, 2800}, .body_q = {14, 12, 10},
        .body_amp = {0.50, 0.30, 0.15}, .radiation = 0.992 },
    [GUN_GRENADE] = {
        .model = GUN_MODEL_PHYSICAL,
        .bore_length_s = 0.01000, .bore_loss = 0.25, .breech_reflect = 0.60,
        .pressure = 1.6, .env_rate = 400.0, .noise_gain = 1.5,
        .body_freq = {80, 250, 1200}, .body_q = {6, 5, 4},
        .body_amp = {0.60, 0.35, 0.15}, .radiation = 0.70,
        .master_amp = 1.6,
        .click_amp = 0.40, .click_decay_ms = 1.0,
        .crack_amp = 0.45, .crack_decay_ms = 25,   .crack_fc = 800,  .crack_q = 0.7,
        .boom_amp  = 1.50, .boom_freq_start = 150, .boom_freq_end = 28,
        .boom_pitch_decay_ms = 60, .boom_amp_decay_ms = 350,
        .tail_amp  = 1.50, .tail_attack_ms = 12,   .tail_decay_ms = 800,
        .tail_fc   = 400,  .tail_q = 0.4 },
    [GUN_RPG] = {
        .model = GUN_MODEL_PHYSICAL,
        .bore_length_s = 0.00300, .bore_loss = 0.30, .breech_reflect = 0.50,
        .pressure = 1.2, .env_rate = 150.0, .noise_gain = 2.5,
        .body_freq = {200, 600, 2000}, .body_q = {4, 3, 3},
        .body_amp = {0.40, 0.30, 0.20}, .radiation = 0.60,
        .secondary_delay_ms = 250, .secondary_amp = 1.5,
        .master_amp = 1.3,
        .click_amp = 0.30, .click_decay_ms = 0.8,
        .crack_amp = 0.40, .crack_decay_ms = 20,   .crack_fc = 1500, .crack_q = 0.8,
        .boom_amp  = 0.30, .boom_freq_start = 120, .boom_freq_end = 60,
        .boom_pitch_decay_ms = 30, .boom_amp_decay_ms = 100,
        .tail_amp  = 2.00, .tail_attack_ms = 80,   .tail_decay_ms = 600,
        .tail_fc   = 600,  .tail_q = 0.5 },
    [GUN_RELOAD] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 0.75,
        .click_amp = 0.85, .click_decay_ms = 0.4,
        .crack_amp = 0.90, .crack_decay_ms = 4.0,  .crack_fc = 4500, .crack_q = 3.0,
        .boom_amp  = 0.0,  .boom_freq_start = 0,   .boom_freq_end = 0,
        .boom_pitch_decay_ms = 1,  .boom_amp_decay_ms = 1,
        .tail_amp  = 0.20, .tail_attack_ms = 0,    .tail_decay_ms = 30,
        .tail_fc   = 2500, .tail_q = 0.6,
        .secondary_delay_ms = 80, .secondary_amp = 0.65,
        .bore_length_s = 0.00010, .bore_loss = 0.70, .breech_reflect = 0.90,
        .pressure = 0.6, .env_rate = 4000.0, .noise_gain = 0.3,
        .body_freq = {2200, 4500, 8000}, .body_q = {10, 8, 6},
        .body_amp = {0.40, 0.30, 0.15}, .radiation = 0.92 },
    [GUN_COCK] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 0.8,
        .click_amp = 0.90, .click_decay_ms = 0.4,
        .crack_amp = 1.00, .crack_decay_ms = 5.0,  .crack_fc = 3800, .crack_q = 3.2,
        .boom_amp  = 0.0,  .boom_freq_start = 0,   .boom_freq_end = 0,
        .boom_pitch_decay_ms = 1,  .boom_amp_decay_ms = 1,
        .tail_amp  = 0.15, .tail_attack_ms = 0,    .tail_decay_ms = 25,
        .tail_fc   = 2000, .tail_q = 0.6,
        .secondary_delay_ms = 55, .secondary_amp = 0.80,
        .bore_length_s = 0.00015, .bore_loss = 0.65, .breech_reflect = 0.88,
        .pressure = 0.7, .env_rate = 3500.0, .noise_gain = 0.35,
        .body_freq = {1800, 4200, 7500}, .body_q = {10, 8, 7},
        .body_amp = {0.45, 0.25, 0.15}, .radiation = 0.92 },
    [GUN_RICOCHET] = {
        .model = GUN_MODEL_CLASSIC, .master_amp = 0.85,
        .click_amp = 0.40, .click_decay_ms = 0.5,
        .crack_amp = 0.35, .crack_decay_ms = 7.0,  .crack_fc = 5500, .crack_q = 3.0,
        .boom_amp  = 0.95, .boom_freq_start = 1800,.boom_freq_end = 1500,
        .boom_pitch_decay_ms = 60, .boom_amp_decay_ms = 350,
        .tail_amp  = 0.20, .tail_attack_ms = 0,    .tail_decay_ms = 200,
        .tail_fc   = 3000, .tail_q = 1.0,
        .bore_length_s = 0.00040, .bore_loss = 0.15, .breech_reflect = 0.90,
        .pressure = 0.8, .env_rate = 600.0, .noise_gain = 0.3,
        .body_freq = {3000, 5500, 9000}, .body_q = {30, 25, 20},
        .body_amp = {0.40, 0.25, 0.15}, .radiation = 0.975 },
};

static inline void compute_resonator(double f, double q, double sr,
                                     double *a1, double *a2, double *b0) {
    if (q < 0.4) q = 0.4;
    if (f < 20.0) f = 20.0;
    if (f > sr * 0.45) f = sr * 0.45;
    double r = exp(-M_PI * f / (q * sr));
    double w = 2.0 * M_PI * f / sr;
    *a1 = 2.0 * r * cos(w);
    *a2 = r * r;
    *b0 = (1.0 - r);
}

static void gun_init_voice(ACVoice *v, GunPreset preset, double sr, int force_model) {
    if (preset < 0 || preset >= GUN_PRESET_COUNT) preset = GUN_PISTOL;
    const GunPresetParams *p = &gun_presets[preset];

    v->gun_preset = (int)preset;
    v->gun_model = (force_model == 0 || force_model == 1) ? force_model : (int)p->model;
    v->gun_pressure = p->master_amp > 0.0 ? p->master_amp : 1.0;
    v->gun_pressure_env = p->sustain_fire ? 0.92 : 1.0;
    v->gun_secondary_trig = p->secondary_delay_ms > 0
                            ? p->secondary_delay_ms * 0.001 * sr : 0.0;
    v->gun_secondary_amp = p->secondary_amp;
    v->gun_sustain_fire = p->sustain_fire;
    v->gun_retrig_timer = 0.0;
    v->gun_retrig_period = p->retrig_period_ms * 0.001;
    v->gun_pitch_mult = 1.0;
    v->gun_pitch_target = 1.0;
    v->gun_pitch_slew = 1.0 / (0.3 * sr);

    if (v->gun_model == GUN_MODEL_CLASSIC) {
        double tau_crack = (p->crack_decay_ms > 0.1 ? p->crack_decay_ms : 0.1) * 0.001;
        v->gun_env_decay_mult = exp(-1.0 / (tau_crack * sr));
        v->gun_boom_freq_start = p->boom_freq_start;
        v->gun_boom_freq_end = p->boom_freq_end;
        v->gun_boom_freq = p->boom_freq_start;
        v->gun_boom_phase = 0.0;
        double tau_pitch = (p->boom_pitch_decay_ms > 0.1 ? p->boom_pitch_decay_ms : 0.1) * 0.001;
        v->gun_boom_pitch_mult = exp(-1.0 / (tau_pitch * sr));
        double tau_boom = (p->boom_amp_decay_ms > 0.1 ? p->boom_amp_decay_ms : 0.1) * 0.001;
        v->gun_boom_decay_mult = exp(-1.0 / (tau_boom * sr));
        v->gun_boom_env = (p->boom_amp > 0.0) ? (p->sustain_fire ? 0.92 : 1.0) : 0.0;
        v->gun_tail_env = (p->tail_attack_ms > 0.0) ? 0.0 : 1.0;
        v->gun_tail_attack_inc = (p->tail_attack_ms > 0.0)
                                 ? 1.0 / (p->tail_attack_ms * 0.001 * sr) : 0.0;
        double tau_tail = (p->tail_decay_ms > 0.1 ? p->tail_decay_ms : 0.1) * 0.001;
        v->gun_tail_decay_mult = exp(-1.0 / (tau_tail * sr));
        compute_resonator(p->crack_fc, p->crack_q, sr,
                          &v->gun_body_a1[0], &v->gun_body_a2[0], &v->gun_crack_b0);
        compute_resonator(p->tail_fc, p->tail_q, sr,
                          &v->gun_body_a1[1], &v->gun_body_a2[1], &v->gun_tail_b0);
        v->gun_tail_b1 = 0.0; v->gun_tail_b2 = 0.0;
        for (int i = 0; i < 3; i++) { v->gun_body_y1[i] = 0.0; v->gun_body_y2[i] = 0.0; }
        v->gun_body_amp[0] = p->crack_amp;
        v->gun_body_amp[1] = p->boom_amp;
        v->gun_body_amp[2] = p->tail_amp;
        v->gun_click_amp = p->click_amp;
        v->gun_click_env = (p->click_amp > 0.0) ? (p->sustain_fire ? 0.92 : 1.0) : 0.0;
        v->gun_click_prev = 0.0;
        double tau_click = (p->click_decay_ms > 0.05 ? p->click_decay_ms : 0.05) * 0.001;
        v->gun_click_decay_mult = exp(-1.0 / (tau_click * sr));
        v->gun_bore_delay = 0.0;
        v->gun_bore_loss = 0.0;
        v->gun_bore_lp = 0.0;
        v->gun_breech_reflect = 0.0;
        v->gun_noise_gain = 0.0;
        v->gun_radiation_a = 0.0;
        v->gun_rad_prev = 0.0;
        memset(v->whistle_bore_buf, 0, sizeof(v->whistle_bore_buf));
        v->whistle_bore_w = 0;
    } else {
        v->gun_bore_delay = p->bore_length_s * sr;
        if (v->gun_bore_delay < 4.0) v->gun_bore_delay = 4.0;
        if (v->gun_bore_delay > 2040.0) v->gun_bore_delay = 2040.0;
        v->gun_bore_loss = p->bore_loss;
        v->gun_bore_lp = 0.0;
        v->gun_breech_reflect = p->breech_reflect;
        v->gun_pressure = p->pressure;
        v->gun_env_decay_mult = exp(-p->env_rate / sr);
        v->gun_noise_gain = p->noise_gain;
        v->gun_radiation_a = p->radiation;
        v->gun_rad_prev = 0.0;
        for (int i = 0; i < 3; i++) {
            double a1, a2, b0_unused;
            compute_resonator(p->body_freq[i], p->body_q[i], sr, &a1, &a2, &b0_unused);
            v->gun_body_a1[i] = a1;
            v->gun_body_a2[i] = a2;
            v->gun_body_amp[i] = p->body_amp[i];
            v->gun_body_y1[i] = 0.0;
            v->gun_body_y2[i] = 0.0;
        }
        memset(v->whistle_bore_buf, 0, sizeof(v->whistle_bore_buf));
        v->whistle_bore_w = 0;
        v->gun_phys_t = 0.0;
        v->gun_phys_t_plus = (3.0 / (p->env_rate > 100 ? p->env_rate : 100.0)) * sr;
        if (v->gun_phys_t_plus < 32.0) v->gun_phys_t_plus = 32.0;
        if (v->gun_phys_t_plus > 4096.0) v->gun_phys_t_plus = 4096.0;
        v->gun_phys_friedlander_a = 1.5;
        v->gun_phys_neg_amp = 0.18;
        v->gun_phys_echo_delay = 0.0035 * sr;
        if (v->gun_phys_echo_delay > 1023.0) v->gun_phys_echo_delay = 1023.0;
        v->gun_phys_echo_amp = 0.22;
        memset(v->gun_phys_echo_buf, 0, sizeof(v->gun_phys_echo_buf));
        v->gun_phys_echo_w = 0;
        v->gun_boom_phase = 0.0;
        v->gun_boom_freq = 0.0;
        v->gun_boom_freq_start = 0.0;
        v->gun_boom_freq_end = 0.0;
        v->gun_boom_pitch_mult = 1.0;
        v->gun_boom_env = 0.0;
        v->gun_boom_decay_mult = 1.0;
        v->gun_tail_env = 0.0;
        v->gun_tail_attack_inc = 0.0;
        v->gun_tail_decay_mult = 1.0;
        v->gun_crack_b0 = 0.0;
        v->gun_tail_b0 = v->gun_tail_b1 = v->gun_tail_b2 = 0.0;
        v->gun_click_amp = 0.0;
        v->gun_click_env = 0.0;
        v->gun_click_decay_mult = 1.0;
        v->gun_click_prev = 0.0;
    }
}

static inline void gun_on_release(ACVoice *v) {
    if (v->type != WAVE_GUN) return;
    if (v->gun_preset == GUN_RICOCHET) {
        v->gun_pitch_target = (v->gun_model == GUN_MODEL_CLASSIC) ? 0.35 : 2.8;
    }
}

// ─── Gun classic (3-layer) ──────────────────────────────────────────────────

static inline double generate_gun_classic_sample(ACVoice *v, double sr) {
    if (v->gun_secondary_trig > 0.0) {
        v->gun_secondary_trig -= 1.0;
        if (v->gun_secondary_trig <= 0.0) {
            v->gun_pressure_env = v->gun_secondary_amp;
            v->gun_boom_env = v->gun_secondary_amp * 0.6;
            v->gun_click_env = v->gun_secondary_amp;
            v->gun_secondary_trig = 0.0;
        }
    }
    if (v->gun_sustain_fire && v->state == VOICE_ACTIVE
        && isinf(v->duration) && v->gun_retrig_period > 0.0) {
        v->gun_retrig_timer += 1.0 / sr;
        if (v->gun_retrig_timer >= v->gun_retrig_period) {
            v->gun_retrig_timer -= v->gun_retrig_period;
            double j = (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
            double jitter = 0.82 + j * 0.32;
            v->gun_pressure_env = jitter;
            v->gun_boom_env = jitter;
            v->gun_click_env = jitter;
            v->gun_boom_freq = v->gun_boom_freq_start;
        }
    }
    if (v->gun_pitch_mult != v->gun_pitch_target) {
        v->gun_pitch_mult += (v->gun_pitch_target - v->gun_pitch_mult) * 0.00012;
    }

    double click = 0.0;
    if (v->gun_click_env > 0.00002 && v->gun_click_amp > 0.0) {
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double hp = white - v->gun_click_prev;
        v->gun_click_prev = white;
        click = hp * v->gun_click_env * v->gun_click_amp;
        v->gun_click_env *= v->gun_click_decay_mult;
    }
    double crack = 0.0;
    if (v->gun_pressure_env > 0.00002 && v->gun_body_amp[0] > 0.0) {
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double y = v->gun_crack_b0 * white
                 + v->gun_body_a1[0] * v->gun_body_y1[0]
                 - v->gun_body_a2[0] * v->gun_body_y2[0];
        v->gun_body_y2[0] = v->gun_body_y1[0];
        v->gun_body_y1[0] = y;
        crack = y * v->gun_pressure_env * v->gun_body_amp[0];
        v->gun_pressure_env *= v->gun_env_decay_mult;
    }
    double boom = 0.0;
    if (v->gun_boom_env > 0.00002 && v->gun_body_amp[1] > 0.0) {
        v->gun_boom_freq = v->gun_boom_freq_end
                         + (v->gun_boom_freq - v->gun_boom_freq_end) * v->gun_boom_pitch_mult;
        double f = v->gun_boom_freq * v->gun_pitch_mult;
        if (f < 1.0) f = 1.0;
        v->gun_boom_phase += f / sr;
        if (v->gun_boom_phase >= 1.0) v->gun_boom_phase -= 1.0;
        if (v->gun_boom_phase < 0.0) v->gun_boom_phase += 1.0;
        double tp = v->gun_boom_phase;
        double s = (tp < 0.5) ? (4.0 * tp - 1.0) : (3.0 - 4.0 * tp);
        boom = s * v->gun_boom_env * v->gun_body_amp[1];
        v->gun_boom_env *= v->gun_boom_decay_mult;
    }
    double tail = 0.0;
    if (v->gun_body_amp[2] > 0.0) {
        if (v->gun_tail_attack_inc > 0.0) {
            v->gun_tail_env += v->gun_tail_attack_inc;
            if (v->gun_tail_env >= 1.0) {
                v->gun_tail_env = 1.0;
                v->gun_tail_attack_inc = 0.0;
            }
        } else if (v->gun_tail_env > 0.00001) {
            v->gun_tail_env *= v->gun_tail_decay_mult;
        }
        if (v->gun_tail_env > 0.00001) {
            double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
            double y = v->gun_tail_b0 * white
                     + v->gun_body_a1[1] * v->gun_body_y1[1]
                     - v->gun_body_a2[1] * v->gun_body_y2[1];
            v->gun_body_y2[1] = v->gun_body_y1[1];
            v->gun_body_y1[1] = y;
            tail = y * v->gun_tail_env * v->gun_body_amp[2];
        }
    }

    double out = (click + crack + boom + tail) * v->gun_pressure;
    return out * synth_compute_envelope(v);
}

// ─── Gun physical (DWG bore + body modes) ───────────────────────────────────

static inline double generate_gun_physical_sample(ACVoice *v, double sr) {
    double t = v->gun_phys_t;
    double t_plus = v->gun_phys_t_plus;
    double A = v->gun_phys_friedlander_a;
    double pulse = 0.0;
    if (t < t_plus) {
        double f = t / t_plus;
        pulse = (1.0 - f) * exp(-A * f);
    } else if (t < t_plus * 5.0) {
        double tn = (t - t_plus) / (t_plus * 4.0);
        pulse = -v->gun_phys_neg_amp * (1.0 - tn) * exp(-2.0 * tn);
    }
    uint32_t n = xorshift32(&v->noise_seed);
    double white = ((double)n / (double)UINT32_MAX) * 2.0 - 1.0;
    double excite = v->gun_pressure * pulse * (1.0 + v->gun_noise_gain * white);
    v->gun_phys_t += 1.0;

    if (v->gun_secondary_trig > 0.0) {
        v->gun_secondary_trig -= 1.0;
        if (v->gun_secondary_trig <= 0.0) {
            v->gun_phys_t = 0.0;
            v->gun_pressure *= v->gun_secondary_amp;
            v->gun_secondary_trig = 0.0;
        }
    }
    if (v->gun_sustain_fire && v->state == VOICE_ACTIVE
        && isinf(v->duration) && v->gun_retrig_period > 0.0) {
        v->gun_retrig_timer += 1.0 / sr;
        if (v->gun_retrig_timer >= v->gun_retrig_period) {
            v->gun_retrig_timer -= v->gun_retrig_period;
            v->gun_phys_t = 0.0;
            double j = (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
            v->gun_pressure *= 0.92 + j * 0.16;
        }
    }
    if (v->gun_pitch_mult != v->gun_pitch_target) {
        v->gun_pitch_mult += (v->gun_pitch_target - v->gun_pitch_mult) * 0.00012;
    }
    double bore_delay = v->gun_bore_delay * v->gun_pitch_mult;
    if (bore_delay < 4.0) bore_delay = 4.0;
    if (bore_delay > 2040.0) bore_delay = 2040.0;

    const int BORE_N = 2048;
    double bore_out = whistle_frac_read(v->whistle_bore_buf, BORE_N, v->whistle_bore_w, bore_delay);
    v->gun_bore_lp = v->gun_bore_loss * (-bore_out)
                   + (1.0 - v->gun_bore_loss) * v->gun_bore_lp;
    double refl = v->gun_bore_lp;
    double into_bore = excite + refl * v->gun_breech_reflect;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)into_bore;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % BORE_N;

    double radiated = into_bore - v->gun_radiation_a * v->gun_rad_prev;
    v->gun_rad_prev = into_bore;

    double body = 0.0;
    for (int i = 0; i < 3; i++) {
        double y = excite + v->gun_body_a1[i] * v->gun_body_y1[i]
                          - v->gun_body_a2[i] * v->gun_body_y2[i];
        v->gun_body_y2[i] = v->gun_body_y1[i];
        v->gun_body_y1[i] = y;
        body += y * v->gun_body_amp[i];
    }
    double dry = radiated * 0.55 + body * 0.45;

    double echo_out = 0.0;
    if (v->gun_phys_echo_amp > 0.0 && v->gun_phys_echo_delay > 1.0) {
        const int ECHO_N = 1024;
        int read_pos = v->gun_phys_echo_w - (int)v->gun_phys_echo_delay;
        while (read_pos < 0) read_pos += ECHO_N;
        echo_out = (double)v->gun_phys_echo_buf[read_pos % ECHO_N] * v->gun_phys_echo_amp;
        v->gun_phys_echo_buf[v->gun_phys_echo_w] = (float)dry;
        v->gun_phys_echo_w = (v->gun_phys_echo_w + 1) % ECHO_N;
    }
    return (dry + echo_out) * synth_compute_envelope(v);
}

static inline double generate_gun_sample(ACVoice *v, double sr) {
    return (v->gun_model == GUN_MODEL_PHYSICAL)
           ? generate_gun_physical_sample(v, sr)
           : generate_gun_classic_sample(v, sr);
}

// ─── Main per-sample generator ──────────────────────────────────────────────

double synth_generate_sample(ACVoice *v, double sample_rate) {
    double s = 0.0;
    switch (v->type) {
    case WAVE_SINE:     s = sin(2.0 * M_PI * v->phase); break;
    case WAVE_SQUARE:   s = v->phase < 0.5 ? 1.0 : -1.0; break;
    case WAVE_TRIANGLE: {
        double tp = v->phase + 0.25;
        if (tp >= 1.0) tp -= 1.0;
        s = 4.0 * fabs(tp - 0.5) - 1.0;
        break;
    }
    case WAVE_SAWTOOTH: s = 2.0 * v->phase - 1.0; break;
    case WAVE_NOISE: {
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double y = v->noise_b0 * white + v->noise_b1 * v->noise_x1 + v->noise_b2 * v->noise_x2
                 - v->noise_a1 * v->noise_y1 - v->noise_a2 * v->noise_y2;
        v->noise_x2 = v->noise_x1; v->noise_x1 = white;
        v->noise_y2 = v->noise_y1; v->noise_y1 = y;
        s = y;
        break;
    }
    case WAVE_WHISTLE: s = generate_whistle_sample(v, sample_rate); break;
    case WAVE_GUN:     s = generate_gun_sample(v, sample_rate); break;
    default: s = 0.0;
    }
    if (v->target_frequency > 0 && v->frequency != v->target_frequency) {
        v->frequency += (v->target_frequency - v->frequency) * 0.0003;
    }
    if (v->type != WAVE_WHISTLE && v->type != WAVE_GUN) {
        v->phase += v->frequency / sample_rate;
        if (v->phase >= 1.0) v->phase -= 1.0;
    }
    return s;
}

void synth_setup_noise_filter(ACVoice *v, double sample_rate) {
    double cutoff = v->frequency;
    if (cutoff < 20.0) cutoff = 20.0;
    if (cutoff > sample_rate / 2.0) cutoff = sample_rate / 2.0;
    double Q = 1.0;
    double w0 = 2.0 * M_PI * cutoff / sample_rate;
    double alpha = sin(w0) / (2.0 * Q);
    double b0 = (1.0 - cos(w0)) / 2.0;
    double b1 = 1.0 - cos(w0);
    double b2 = (1.0 - cos(w0)) / 2.0;
    double a0 = 1.0 + alpha;
    double a1 = -2.0 * cos(w0);
    double a2 = 1.0 - alpha;
    v->noise_b0 = b0 / a0;
    v->noise_b1 = b1 / a0;
    v->noise_b2 = b2 / a0;
    v->noise_a1 = a1 / a0;
    v->noise_a2 = a2 / a0;
    v->noise_x1 = v->noise_x2 = v->noise_y1 = v->noise_y2 = 0.0;
}

// ─── Voice lifecycle ────────────────────────────────────────────────────────

void synth_core_init(SynthCore *s, ACVoice *voices, int max_voices,
                     pthread_mutex_t *lock, uint64_t *next_id, double sr) {
    s->voices = voices;
    s->max_voices = max_voices;
    s->lock = lock;
    s->next_id = next_id;
    s->sample_rate = sr;
    if (s->next_id && *s->next_id == 0) *s->next_id = 1;
}

uint64_t synth_synth(SynthCore *s, WaveType type, double freq,
                     double duration, double volume, double attack,
                     double decay, double pan) {
    if (!s) return 0;
    pthread_mutex_lock(s->lock);
    int slot = -1;
    for (int i = 0; i < s->max_voices; i++) {
        if (s->voices[i].state == VOICE_INACTIVE) { slot = i; break; }
    }
    if (slot < 0) {
        double oldest = 0; slot = 0;
        for (int i = 0; i < s->max_voices; i++) {
            if (s->voices[i].elapsed > oldest) { oldest = s->voices[i].elapsed; slot = i; }
        }
    }
    ACVoice *v = &s->voices[slot];
    memset(v, 0, sizeof(ACVoice));
    v->state = VOICE_ACTIVE;
    v->type = type;
    v->phase = 0.0;
    v->frequency = freq;
    v->target_frequency = freq;
    v->volume = volume;
    v->pan = pan;
    v->attack = attack > 0 ? attack : 0.005;
    v->decay = decay > 0 ? decay : 0.1;
    v->duration = duration;
    v->id = ++(*s->next_id);

    if (type == WAVE_NOISE || type == WAVE_WHISTLE || type == WAVE_GUN) {
        v->noise_seed = (uint32_t)(*s->next_id * 2654435761u);
    }
    if (type == WAVE_NOISE) {
        synth_setup_noise_filter(v, s->sample_rate);
    } else if (type == WAVE_WHISTLE) {
        memset(v->whistle_bore_buf, 0, sizeof(v->whistle_bore_buf));
        memset(v->whistle_jet_buf, 0, sizeof(v->whistle_jet_buf));
        v->whistle_bore_w = 0;
        v->whistle_jet_w = 0;
        v->whistle_breath = 0.0;
        v->whistle_vibrato_phase = 0.0;
        v->whistle_lp1 = 0.0;
        v->whistle_hp_x1 = 0.0;
        v->whistle_hp_y1 = 0.0;
    }
    uint64_t id = v->id;
    pthread_mutex_unlock(s->lock);
    return id;
}

uint64_t synth_synth_gun(SynthCore *s, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model) {
    if (!s) return 0;
    uint64_t id = synth_synth(s, WAVE_GUN, 110.0, duration, volume, attack, decay, pan);
    if (!id) return 0;
    pthread_mutex_lock(s->lock);
    for (int i = 0; i < s->max_voices; i++) {
        if (s->voices[i].id == id) {
            gun_init_voice(&s->voices[i], preset, s->sample_rate, force_model);
            if (pressure_scale > 0.0 && pressure_scale != 1.0) {
                s->voices[i].gun_pressure *= pressure_scale;
            }
            break;
        }
    }
    pthread_mutex_unlock(s->lock);
    return id;
}

void synth_kill(SynthCore *s, uint64_t id, double fade) {
    if (!s) return;
    pthread_mutex_lock(s->lock);
    for (int i = 0; i < s->max_voices; i++) {
        if (s->voices[i].id == id && s->voices[i].state == VOICE_ACTIVE) {
            s->voices[i].state = VOICE_KILLING;
            s->voices[i].fade_duration = fade > 0 ? fade : 0.025;
            s->voices[i].fade_elapsed = 0.0;
            if (s->voices[i].type == WAVE_GUN) gun_on_release(&s->voices[i]);
            break;
        }
    }
    pthread_mutex_unlock(s->lock);
}

void synth_update(SynthCore *s, uint64_t id, double freq, double vol, double pan) {
    if (!s) return;
    pthread_mutex_lock(s->lock);
    for (int i = 0; i < s->max_voices; i++) {
        ACVoice *v = &s->voices[i];
        if (v->id == id && v->state != VOICE_INACTIVE) {
            if (freq > 0)    v->target_frequency = freq;
            if (vol  >= 0)   v->volume = vol;
            if (pan  > -2.0) v->pan = pan;
            break;
        }
    }
    pthread_mutex_unlock(s->lock);
}

void synth_gun_set_param(SynthCore *s, uint64_t id, const char *key, double value) {
    if (!s || !key) return;
    pthread_mutex_lock(s->lock);
    ACVoice *v = NULL;
    for (int i = 0; i < s->max_voices; i++) {
        if (s->voices[i].id == id && s->voices[i].type == WAVE_GUN) {
            v = &s->voices[i]; break;
        }
    }
    if (!v) { pthread_mutex_unlock(s->lock); return; }
    double sr = s->sample_rate;
    if (v->gun_model == GUN_MODEL_CLASSIC) {
        if      (strcmp(key, "click_amp") == 0) v->gun_click_amp = value;
        else if (strcmp(key, "click_decay_ms") == 0) {
            double tau = (value > 0.05 ? value : 0.05) * 0.001;
            v->gun_click_decay_mult = exp(-1.0 / (tau * sr));
        }
        else if (strcmp(key, "crack_amp") == 0) v->gun_body_amp[0] = value;
        else if (strcmp(key, "crack_decay_ms") == 0) {
            double tau = (value > 0.1 ? value : 0.1) * 0.001;
            v->gun_env_decay_mult = exp(-1.0 / (tau * sr));
        }
        else if (strcmp(key, "crack_fc") == 0 || strcmp(key, "crack_q") == 0) {
            double a2 = v->gun_body_a2[0];
            double r = a2 > 0 ? sqrt(a2) : 0.95;
            double cur_w = acos(v->gun_body_a1[0] / (2.0 * r));
            double cur_f = cur_w * sr / (2.0 * M_PI);
            double cur_q = -M_PI * cur_f / (sr * log(r > 0.0001 ? r : 0.0001));
            double f = (strcmp(key, "crack_fc") == 0) ? value : cur_f;
            double q = (strcmp(key, "crack_q") == 0) ? value : cur_q;
            compute_resonator(f, q, sr, &v->gun_body_a1[0], &v->gun_body_a2[0], &v->gun_crack_b0);
        }
        else if (strcmp(key, "boom_amp") == 0) v->gun_body_amp[1] = value;
        else if (strcmp(key, "boom_freq_start") == 0) {
            v->gun_boom_freq_start = value; v->gun_boom_freq = value;
        }
        else if (strcmp(key, "boom_freq_end") == 0) v->gun_boom_freq_end = value;
        else if (strcmp(key, "boom_pitch_decay_ms") == 0) {
            double tau = (value > 0.1 ? value : 0.1) * 0.001;
            v->gun_boom_pitch_mult = exp(-1.0 / (tau * sr));
        }
        else if (strcmp(key, "boom_amp_decay_ms") == 0) {
            double tau = (value > 0.1 ? value : 0.1) * 0.001;
            v->gun_boom_decay_mult = exp(-1.0 / (tau * sr));
        }
        else if (strcmp(key, "tail_amp") == 0) v->gun_body_amp[2] = value;
        else if (strcmp(key, "tail_decay_ms") == 0) {
            double tau = (value > 0.1 ? value : 0.1) * 0.001;
            v->gun_tail_decay_mult = exp(-1.0 / (tau * sr));
        }
        else if (strcmp(key, "tail_fc") == 0 || strcmp(key, "tail_q") == 0) {
            double a2 = v->gun_body_a2[1];
            double r = a2 > 0 ? sqrt(a2) : 0.95;
            double cur_w = acos(v->gun_body_a1[1] / (2.0 * r));
            double cur_f = cur_w * sr / (2.0 * M_PI);
            double cur_q = -M_PI * cur_f / (sr * log(r > 0.0001 ? r : 0.0001));
            double f = (strcmp(key, "tail_fc") == 0) ? value : cur_f;
            double q = (strcmp(key, "tail_q") == 0) ? value : cur_q;
            compute_resonator(f, q, sr, &v->gun_body_a1[1], &v->gun_body_a2[1], &v->gun_tail_b0);
        }
    } else {
        if      (strcmp(key, "pressure") == 0) v->gun_pressure = value;
        else if (strcmp(key, "env_rate") == 0) {
            v->gun_phys_t_plus = (3.0 / (value > 100 ? value : 100.0)) * sr;
            if (v->gun_phys_t_plus < 32.0)   v->gun_phys_t_plus = 32.0;
            if (v->gun_phys_t_plus > 4096.0) v->gun_phys_t_plus = 4096.0;
        }
        else if (strcmp(key, "bore_length_s") == 0) {
            v->gun_bore_delay = value * sr;
            if (v->gun_bore_delay < 4.0)    v->gun_bore_delay = 4.0;
            if (v->gun_bore_delay > 2040.0) v->gun_bore_delay = 2040.0;
        }
        else if (strcmp(key, "bore_loss") == 0)      v->gun_bore_loss = value;
        else if (strcmp(key, "breech_reflect") == 0) v->gun_breech_reflect = value;
        else if (strcmp(key, "noise_gain") == 0)     v->gun_noise_gain = value;
        else if (strcmp(key, "radiation") == 0)      v->gun_radiation_a = value;
        else if (strncmp(key, "body_freq", 9) == 0 || strncmp(key, "body_q", 6) == 0) {
            int idx = key[strlen(key) - 1] - '0';
            if (idx >= 0 && idx <= 2) {
                double a2 = v->gun_body_a2[idx];
                double r = a2 > 0 ? sqrt(a2) : 0.95;
                double cur_w = acos(v->gun_body_a1[idx] / (2.0 * r));
                double cur_f = cur_w * sr / (2.0 * M_PI);
                double cur_q = -M_PI * cur_f / (sr * log(r > 0.0001 ? r : 0.0001));
                double f = (strncmp(key, "body_freq", 9) == 0) ? value : cur_f;
                double q = (strncmp(key, "body_q", 6) == 0) ? value : cur_q;
                double b0_unused;
                compute_resonator(f, q, sr, &v->gun_body_a1[idx], &v->gun_body_a2[idx], &b0_unused);
            }
        }
        else if (strncmp(key, "body_amp", 8) == 0) {
            int idx = key[strlen(key) - 1] - '0';
            if (idx >= 0 && idx <= 2) v->gun_body_amp[idx] = value;
        }
    }
    pthread_mutex_unlock(s->lock);
}

// ─── Render loop ────────────────────────────────────────────────────────────

void synth_render(SynthCore *s, float *out, int frames) {
    if (!s || frames <= 0 || !out) return;
    pthread_mutex_lock(s->lock);
    double sr = s->sample_rate;
    double dt = 1.0 / sr;
    for (int f = 0; f < frames; f++) {
        double l = 0.0, r = 0.0;
        for (int i = 0; i < s->max_voices; i++) {
            ACVoice *v = &s->voices[i];
            if (v->state == VOICE_INACTIVE) continue;
            double samp = synth_generate_sample(v, sr);
            double env  = synth_compute_envelope(v);
            double fade = synth_compute_fade(v);
            double amp  = samp * env * fade * v->volume;
            double theta = (v->pan + 1.0) * (M_PI / 4.0);
            l += amp * cos(theta);
            r += amp * sin(theta);
            v->elapsed += dt;
            if (v->state == VOICE_KILLING) {
                v->fade_elapsed += dt;
                if (v->fade_elapsed >= v->fade_duration) v->state = VOICE_INACTIVE;
            } else if (!isinf(v->duration) && v->elapsed >= v->duration) {
                v->state = VOICE_INACTIVE;
            }
        }
        out[f * 2 + 0] += (float)l;
        out[f * 2 + 1] += (float)r;
    }
    pthread_mutex_unlock(s->lock);
}
