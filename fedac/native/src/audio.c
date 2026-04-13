// audio.c — ALSA sound engine for ac-native
// Dedicated audio thread with multi-voice synthesis, envelopes, and effects.

#include "audio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <dirent.h>
#include <unistd.h>
#include <sched.h>
#include <alsa/asoundlib.h>

// Defined in ac-native.c — writes to USB log and stderr.
extern void ac_log(const char *fmt, ...);

// Forward declarations
static int read_system_volume_card(int card);

// ============================================================
// Note frequency table (octave 0 base frequencies)
// ============================================================

static const struct { const char *name; double freq; } note_table[] = {
    {"c",  16.3516}, {"cs", 17.3239}, {"db", 17.3239},
    {"d",  18.3540}, {"ds", 19.4454}, {"eb", 19.4454},
    {"e",  20.6017}, {"f",  21.8268}, {"fs", 23.1247},
    {"gb", 23.1247}, {"g",  24.4997}, {"gs", 25.9565},
    {"ab", 25.9565}, {"a",  27.5000}, {"as", 29.1352},
    {"bb", 29.1352}, {"b",  30.8677},
};
#define NOTE_TABLE_SIZE (sizeof(note_table) / sizeof(note_table[0]))

double audio_note_to_freq(const char *note) {
    if (!note || !*note) return 440.0;

    // Try parsing as a number first
    char *end;
    double d = strtod(note, &end);
    if (end != note && *end == '\0') return d;

    // Parse note string: "C4", "4C#", "C#4", "5A", etc.
    int octave = 4;
    char name_buf[8] = {0};
    int ni = 0;
    const char *p = note;

    // Check if starts with digit (octave prefix: "4C#")
    if (*p >= '0' && *p <= '9') {
        octave = *p - '0';
        p++;
    }

    // Read note name
    while (*p && ni < 3) {
        char ch = *p;
        if (ch >= 'A' && ch <= 'G') ch += 32; // lowercase
        if ((ch >= 'a' && ch <= 'g') || ch == '#' || ch == 's' || ch == 'b') {
            // Map 'f' for flat and '#' for sharp
            if (ch == '#') { name_buf[ni++] = 's'; }
            else { name_buf[ni++] = ch; }
            p++;
        } else break;
    }
    name_buf[ni] = '\0';

    // Trailing octave number
    if (*p >= '0' && *p <= '9') {
        octave = *p - '0';
    }

    // Lookup base frequency
    double base = 440.0; // fallback
    for (int i = 0; i < (int)NOTE_TABLE_SIZE; i++) {
        if (strcmp(name_buf, note_table[i].name) == 0) {
            base = note_table[i].freq;
            break;
        }
    }

    return base * pow(2.0, octave);
}

// ============================================================
// Oscillator sample generation
// ============================================================

static inline uint32_t xorshift32(uint32_t *state) {
    uint32_t x = *state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    *state = x;
    return x;
}

static inline double clampd(double x, double lo, double hi) {
    if (x < lo) return lo;
    if (x > hi) return hi;
    return x;
}

static inline double compute_envelope(ACVoice *v) {
    double env = 1.0;

    // Attack ramp
    if (v->attack > 0.0 && v->elapsed < v->attack) {
        env = v->elapsed / v->attack;
    }

    // Decay (near end of duration)
    if (!isinf(v->duration) && v->decay > 0.0) {
        double decay_start = v->duration - v->decay;
        if (decay_start < 0.0) decay_start = 0.0;
        if (v->elapsed > decay_start) {
            double decay_progress = (v->elapsed - decay_start) / v->decay;
            if (decay_progress > 1.0) decay_progress = 1.0;
            env *= (1.0 - decay_progress);
        }
    }

    return env;
}

// Fractional-delay read from a ring buffer. `delay` is in samples, allows
// non-integer values via linear interpolation between adjacent samples.
// Returns the sample `delay` positions behind the write cursor.
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

// Cook/STK digital waveguide flute model.
// The signal flow (see reports/research for full derivation):
//
//    breath ──► (+) ──► jetDelay ──► NL(x*(x*x-1)) ──► dcBlock ──► (+) ──► boreDelay ──┬──► out
//                ▲                                                  ▲                  │
//                │ −jetRefl·temp                                    │ +endRefl·temp    │
//                │                                                  │                  │
//                └───────── 1-pole LPF ◄───────────────────────────┴──────────────────┘
//
// The BORE delay line (length = SR/freq) is the primary resonator. Its
// closed-loop feedback generates ALL harmonics automatically via comb
// filtering — the delay line is inherently a periodic waveguide that
// sustains exactly at integer multiples of its natural pitch.
//
// The JET delay (length ≈ 0.32 × bore) models the air jet's travel time
// across the embouchure hole. The cubic nonlinearity x*(x*x-1) has
// negative-slope region at x=0 which makes it a LIMIT-CYCLE GENERATOR —
// it converts steady DC breath pressure into sustained oscillation.
// This is qualitatively different from tanh, which is monotonic and
// can only saturate.
//
// The 1-pole LPF in the loop models bore losses (viscothermal damping)
// so the tone darkens as harmonics decay faster than the fundamental.
//
// The DC blocker after the NL removes the bias the cubic would pump
// into the bore loop, which would otherwise drive it into clipping.
static inline double generate_whistle_sample(ACVoice *v, double sample_rate) {
    double env = compute_envelope(v);
    // Breath envelope — DC pressure component + noise modulation + vibrato.
    // CRITICAL: the DC component is what drives the nonlinearity into
    // self-oscillation. Without a steady DC term, noise alone cannot
    // sustain the limit cycle.
    double breath_target = 0.18 + 0.82 * sqrt(env);
    double breath_slew = env > v->whistle_breath ? 0.012 : 0.003;
    v->whistle_breath += (breath_target - v->whistle_breath) * breath_slew;

    // Vibrato LFO — ~5 Hz, small depth
    v->whistle_vibrato_phase += 5.0 / sample_rate;
    if (v->whistle_vibrato_phase >= 1.0) v->whistle_vibrato_phase -= 1.0;
    double vibrato = sin(2.0 * M_PI * v->whistle_vibrato_phase) * 0.03;

    // Breath noise — multiplicatively modulates the DC breath pressure.
    // Low gain so the noise rides on top of the steady breath instead of
    // replacing it. Attack phase gets slightly more chiff.
    double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
    double onset = 1.0 - env;
    double noise_gain = 0.08 + 0.05 * onset;
    double breath = v->whistle_breath * (1.0 + noise_gain * white + vibrato);

    // Bore and jet delay lengths — bore = SR/freq (one wavelength),
    // jet = 0.32 × bore (Cook's flute ratio; 0.45 for pennywhistle,
    // 0.5 for ocarina). Clamp to the delay buffer sizes.
    double freq = clampd(v->frequency, 110.0, sample_rate * 0.20);
    double bore_delay = sample_rate / freq;
    double jet_delay = bore_delay * 0.32;
    // Cap to buffer sizes with safety margin
    const int BORE_N = 2048;
    const int JET_N = 512;
    if (bore_delay > (double)(BORE_N - 2)) bore_delay = (double)(BORE_N - 2);
    if (jet_delay > (double)(JET_N - 2)) jet_delay = (double)(JET_N - 2);

    // Read bore output and apply 1-pole loop LPF (models bore damping).
    // 0.35/0.65 coefficients give ~0.65 DC gain — closes the loop just
    // under unity so it sustains but doesn't blow up. The LPF rolls off
    // high harmonics so the tone darkens naturally, unlike a biquad
    // which would over-narrow the spectrum.
    double bore_out = whistle_frac_read(v->whistle_bore_buf, BORE_N, v->whistle_bore_w, bore_delay);
    v->whistle_lp1 = 0.35 * (-bore_out) + 0.65 * v->whistle_lp1;
    double temp = v->whistle_lp1;

    // Jet drive: breath pressure minus jet reflection from bore feedback
    double jet_refl = 0.5;
    double end_refl = 0.5;
    double pd = breath - jet_refl * temp;

    // Write to jet delay, read back with fractional delay
    v->whistle_jet_buf[v->whistle_jet_w] = (float)pd;
    v->whistle_jet_w = (v->whistle_jet_w + 1) % JET_N;
    pd = whistle_frac_read(v->whistle_jet_buf, JET_N, v->whistle_jet_w, jet_delay);

    // THE CUBIC NONLINEARITY — y = x*(x*x - 1). Negative slope at x=0
    // creates a limit-cycle generator. This is the secret sauce that
    // makes the tone WHISTLE instead of being filtered noise.
    pd = pd * (pd * pd - 1.0);
    if (pd > 1.0) pd = 1.0;
    if (pd < -1.0) pd = -1.0;

    // 1-pole DC blocker — removes the bias the cubic pumps into the loop.
    // y[n] = x[n] - x[n-1] + 0.995*y[n-1]
    double y = pd - v->whistle_hp_x1 + 0.995 * v->whistle_hp_y1;
    v->whistle_hp_x1 = pd;
    v->whistle_hp_y1 = y;

    // Close the bore loop: combine the NL-filtered jet output with the
    // end reflection from the bore delay.
    double into_bore = y + end_refl * temp;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)into_bore;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % BORE_N;

    // Output is a tap off the bore loop. 0.3 gain matches STK Flute.
    return 0.3 * into_bore;
}

// ============================================================
// Gun synthesis — two models per preset
// ============================================================
//
// CLASSIC (default) — three-layer kick+snare-style synthesis:
//
//   noise ──► BPF (mid-Q, ~2-6kHz) ──► amp_env(crack) ──┐
//                                                       │
//   sin/tri ──► pitch_sweep(start→end) ──► amp_env(boom)─┼─► sum ──► out
//                                                       │
//   noise ──► LPF (low-Q, ~200-2000Hz) ──► env(attack+decay)─┘
//
//   • crack: instantaneous transient, exp decay 5-30 ms
//   • boom: pitched sine/triangle with fast downward sweep
//     (~250→40 Hz over 30-100 ms), exp amp decay
//   • tail: noisy residual rumble, optional linear attack ramp,
//     exp decay 100-800 ms
//
//   This is how kick+snare drum synthesis works, applied to gunshots.
//   Cheap, predictable, sounds like the gunshot SFX you remember from
//   classic sample libraries and arcade games.
//
// PHYSICAL — digital waveguide barrel resonance + body modes:
//
//   excitation ──► (+) ──► boreDelay ─┬──► muzzleHPF ──► out
//                   ▲                 │
//                   │ breech_reflect  │
//                   │                 ▼
//                   └─── boreLP ◄──── (−1 open-end refl)
//
//   excitation ──► 3× bodyModes ──► +out (parallel)
//
//   Bore length sets the cavity resonance ("boom" frequency); body
//   modes give metallic character. Better for cavity-dominated sounds
//   (grenade, RPG launch) where the bore behavior actually matters.
//
// Common to both: secondary trigger (N-wave / 2nd click), sustain fire
// (LMG retrigger), ricochet pitch sweep on release.
//
// Bore buffer is SHARED with the whistle (whistle_bore_buf) — only the
// physical model uses it.

typedef struct {
    GunModel model;
    // --- Common (both models) ---
    double master_amp;          // overall layer scaling (0.4–2.0)
    double secondary_delay_ms;  // 0 = no 2nd shot; else delay before re-trigger
    double secondary_amp;       // amplitude of 2nd shot relative to primary
    int    sustain_fire;        // 1 = retrigger while held (LMG)
    double retrig_period_ms;    // ms between retrigs (60000/RPM)
    // --- Classic-only ---
    double click_amp;           // sub-ms HF transient gain (0=off, ~0.6 typical)
    double click_decay_ms;      // very fast (0.3-0.8 ms) — the "tk" snap
    double crack_amp;           // 0..1 mix gain
    double crack_decay_ms;      // exp decay time of crack envelope
    double crack_fc;            // BPF center Hz (2000-8000 typical)
    double crack_q;             // BPF Q (1.0-3.0 typical)
    double boom_amp;            // 0..1 mix gain
    double boom_freq_start;     // Hz at trigger
    double boom_freq_end;       // Hz settled (≈40-80)
    double boom_pitch_decay_ms; // time const for pitch sweep (10-50)
    double boom_amp_decay_ms;   // amp decay time (30-200)
    double tail_amp;            // 0..1 mix gain
    double tail_attack_ms;      // 0 = instant
    double tail_decay_ms;       // 100-800
    double tail_fc;             // LPF cutoff Hz (200-2000)
    double tail_q;              // LPF Q (0.5-1.5)
    // --- Physical-only ---
    double bore_length_s;       // seconds (= 2L/c)
    double bore_loss;           // bore LPF alpha
    double breech_reflect;      // 0..1
    double pressure;            // excitation peak
    double env_rate;            // excitation decay rate (1/sec)
    double noise_gain;          // turbulent noise on excitation
    double body_freq[3];        // mode freqs Hz
    double body_q[3];           // mode Q
    double body_amp[3];         // mode mix amplitudes
    double radiation;           // muzzle HPF coeff
} GunPresetParams;

// Per-weapon parameters. Most presets use CLASSIC for clean impact
// sounds. Cavity-dominated weapons (grenade, RPG) keep the PHYSICAL
// bore model where its long resonance helps.
static const GunPresetParams gun_presets[GUN_PRESET_COUNT] = {
    // --- GUN_PISTOL (9mm, L≈100mm) — sharp crack, tiny sub, quick tail
    { .model = GUN_MODEL_CLASSIC, .master_amp = 1.1,
      .click_amp = 0.65, .click_decay_ms = 0.5,
      .crack_amp = 0.95, .crack_decay_ms = 7.0,  .crack_fc = 3800, .crack_q = 2.6,
      .boom_amp  = 0.55, .boom_freq_start = 220, .boom_freq_end = 55,
      .boom_pitch_decay_ms = 14, .boom_amp_decay_ms = 55,
      .tail_amp  = 0.35, .tail_attack_ms = 0,    .tail_decay_ms = 110,
      .tail_fc   = 900,  .tail_q = 0.8,
      // Physical alt (warB): short barrel, bright body modes
      .bore_length_s = 0.000588, .bore_loss = 0.55, .breech_reflect = 0.92,
      .pressure = 1.2, .env_rate = 3000.0, .noise_gain = 0.6,
      .body_freq = {1500, 4000, 8500}, .body_q = {12, 10, 8},
      .body_amp = {0.30, 0.20, 0.15}, .radiation = 0.985 },
    // --- GUN_RIFLE (AR-15, L≈400mm) — bright crack + supersonic N-wave tap
    { .model = GUN_MODEL_CLASSIC, .master_amp = 1.2,
      .click_amp = 0.75, .click_decay_ms = 0.6,
      .crack_amp = 1.05, .crack_decay_ms = 8.0,  .crack_fc = 4500, .crack_q = 3.0,
      .boom_amp  = 0.70, .boom_freq_start = 280, .boom_freq_end = 50,
      .boom_pitch_decay_ms = 18, .boom_amp_decay_ms = 90,
      .tail_amp  = 0.45, .tail_attack_ms = 0,    .tail_decay_ms = 220,
      .tail_fc   = 1100, .tail_q = 0.7,
      .secondary_delay_ms = 0.9, .secondary_amp = 0.55,
      // Physical alt: longer bore, deep mode ring + N-wave secondary
      .bore_length_s = 0.00235, .bore_loss = 0.50, .breech_reflect = 0.95,
      .pressure = 1.5, .env_rate = 2500.0, .noise_gain = 0.5,
      .body_freq = {800, 2400, 6000}, .body_q = {14, 12, 10},
      .body_amp = {0.35, 0.25, 0.15}, .radiation = 0.988 },
    // --- GUN_SHOTGUN (12ga, L≈660mm, wide bore) — big low boom, noisy tail
    { .model = GUN_MODEL_CLASSIC, .master_amp = 1.4,
      .click_amp = 0.55, .click_decay_ms = 0.8,
      .crack_amp = 0.65, .crack_decay_ms = 12,   .crack_fc = 2200, .crack_q = 1.8,
      .boom_amp  = 1.10, .boom_freq_start = 260, .boom_freq_end = 38,
      .boom_pitch_decay_ms = 22, .boom_amp_decay_ms = 130,
      .tail_amp  = 0.85, .tail_attack_ms = 4,    .tail_decay_ms = 380,
      .tail_fc   = 700,  .tail_q = 0.6,
      // Physical alt: wide bore, low body modes
      .bore_length_s = 0.00388, .bore_loss = 0.40, .breech_reflect = 0.88,
      .pressure = 1.8, .env_rate = 1800.0, .noise_gain = 0.9,
      .body_freq = {400, 1200, 3500}, .body_q = {10, 8, 7},
      .body_amp = {0.40, 0.25, 0.15}, .radiation = 0.965 },
    // --- GUN_SMG (MP5, L≈225mm) — bright fast crack, mild boom
    { .model = GUN_MODEL_CLASSIC, .master_amp = 1.0,
      .click_amp = 0.65, .click_decay_ms = 0.4,
      .crack_amp = 0.95, .crack_decay_ms = 5.0,  .crack_fc = 4200, .crack_q = 2.5,
      .boom_amp  = 0.45, .boom_freq_start = 200, .boom_freq_end = 60,
      .boom_pitch_decay_ms = 10, .boom_amp_decay_ms = 40,
      .tail_amp  = 0.30, .tail_attack_ms = 0,    .tail_decay_ms = 90,
      .tail_fc   = 1200, .tail_q = 0.7,
      // Physical alt
      .bore_length_s = 0.00132, .bore_loss = 0.58, .breech_reflect = 0.92,
      .pressure = 1.0, .env_rate = 3500.0, .noise_gain = 0.5,
      .body_freq = {1200, 3500, 7500}, .body_q = {12, 10, 8},
      .body_amp = {0.30, 0.20, 0.13}, .radiation = 0.978 },
    // --- GUN_SUPPRESSED — tiny click, no boom, mid-range "pfft"
    { .model = GUN_MODEL_CLASSIC, .master_amp = 0.7,
      .click_amp = 0.08, .click_decay_ms = 0.4,
      .crack_amp = 0.30, .crack_decay_ms = 6.0,  .crack_fc = 1600, .crack_q = 1.1,
      .boom_amp  = 0.10, .boom_freq_start = 150, .boom_freq_end = 80,
      .boom_pitch_decay_ms = 8,  .boom_amp_decay_ms = 30,
      .tail_amp  = 0.85, .tail_attack_ms = 6,    .tail_decay_ms = 140,
      .tail_fc   = 1800, .tail_q = 0.6,
      // Physical alt: heavy bore loss = absorptive baffles, low radiation
      .bore_length_s = 0.00100, .bore_loss = 0.85, .breech_reflect = 0.80,
      .pressure = 0.5, .env_rate = 1500.0, .noise_gain = 1.0,
      .body_freq = {600, 1500, 3000}, .body_q = {6, 5, 4},
      .body_amp = {0.15, 0.10, 0.05}, .radiation = 0.85 },
    // --- GUN_LMG (M60, L≈560mm) — rifle-class retriggered ~600 RPM
    { .model = GUN_MODEL_CLASSIC, .master_amp = 1.15,
      .click_amp = 0.65, .click_decay_ms = 0.5,
      .crack_amp = 0.95, .crack_decay_ms = 7.0,  .crack_fc = 3500, .crack_q = 2.6,
      .boom_amp  = 0.75, .boom_freq_start = 250, .boom_freq_end = 48,
      .boom_pitch_decay_ms = 16, .boom_amp_decay_ms = 75,
      .tail_amp  = 0.45, .tail_attack_ms = 0,    .tail_decay_ms = 160,
      .tail_fc   = 950,  .tail_q = 0.7,
      .sustain_fire = 1, .retrig_period_ms = 100,  // 600 RPM
      // Physical alt
      .bore_length_s = 0.00329, .bore_loss = 0.48, .breech_reflect = 0.94,
      .pressure = 1.4, .env_rate = 2200.0, .noise_gain = 0.55,
      .body_freq = {600, 1800, 4500}, .body_q = {12, 10, 8},
      .body_amp = {0.35, 0.25, 0.15}, .radiation = 0.982 },
    // --- GUN_SNIPER (.50, L≈740mm) — huge crack + N-wave + long tail
    { .model = GUN_MODEL_CLASSIC, .master_amp = 1.5,
      .click_amp = 0.85, .click_decay_ms = 0.7,
      .crack_amp = 1.20, .crack_decay_ms = 11,   .crack_fc = 5000, .crack_q = 3.2,
      .boom_amp  = 1.20, .boom_freq_start = 320, .boom_freq_end = 36,
      .boom_pitch_decay_ms = 28, .boom_amp_decay_ms = 180,
      .tail_amp  = 0.70, .tail_attack_ms = 3,    .tail_decay_ms = 500,
      .tail_fc   = 850,  .tail_q = 0.8,
      .secondary_delay_ms = 1.4, .secondary_amp = 0.70,
      // Physical alt: high pressure, long ring
      .bore_length_s = 0.00435, .bore_loss = 0.35, .breech_reflect = 0.97,
      .pressure = 2.0, .env_rate = 1500.0, .noise_gain = 0.7,
      .body_freq = {350, 950, 2800}, .body_q = {14, 12, 10},
      .body_amp = {0.50, 0.30, 0.15}, .radiation = 0.992 },
    // --- GUN_GRENADE — large cavity, slow release. Default = PHYSICAL
    // (the long bore resonance makes the cavity feel right). Classic
    // alt is a very low boom + heavy noisy tail for the kaboom.
    { .model = GUN_MODEL_PHYSICAL,
      .bore_length_s = 0.01000, .bore_loss = 0.25, .breech_reflect = 0.60,
      .pressure = 1.6, .env_rate = 400.0, .noise_gain = 1.5,
      .body_freq = {80, 250, 1200}, .body_q = {6, 5, 4},
      .body_amp = {0.60, 0.35, 0.15}, .radiation = 0.70,
      // Classic alt (warA): tiny click, huge boom, very long tail
      .master_amp = 1.6,
      .click_amp = 0.40, .click_decay_ms = 1.0,
      .crack_amp = 0.45, .crack_decay_ms = 25,   .crack_fc = 800,  .crack_q = 0.7,
      .boom_amp  = 1.50, .boom_freq_start = 150, .boom_freq_end = 28,
      .boom_pitch_decay_ms = 60, .boom_amp_decay_ms = 350,
      .tail_amp  = 1.50, .tail_attack_ms = 12,   .tail_decay_ms = 800,
      .tail_fc   = 400,  .tail_q = 0.4 },
    // --- GUN_RPG — long motor burn + delayed boom. Default = PHYSICAL
    // (the slow bore loop nicely models the rocket exhaust whoosh).
    { .model = GUN_MODEL_PHYSICAL,
      .bore_length_s = 0.00300, .bore_loss = 0.30, .breech_reflect = 0.50,
      .pressure = 1.2, .env_rate = 150.0, .noise_gain = 2.5,
      .body_freq = {200, 600, 2000}, .body_q = {4, 3, 3},
      .body_amp = {0.40, 0.30, 0.20}, .radiation = 0.60,
      .secondary_delay_ms = 250, .secondary_amp = 1.5,
      // Classic alt: launch click, sustained noise (motor) + delayed boom
      .master_amp = 1.3,
      .click_amp = 0.30, .click_decay_ms = 0.8,
      .crack_amp = 0.40, .crack_decay_ms = 20,   .crack_fc = 1500, .crack_q = 0.8,
      .boom_amp  = 0.30, .boom_freq_start = 120, .boom_freq_end = 60,
      .boom_pitch_decay_ms = 30, .boom_amp_decay_ms = 100,
      .tail_amp  = 2.00, .tail_attack_ms = 80,   .tail_decay_ms = 600,
      .tail_fc   = 600,  .tail_q = 0.5 },
    // --- GUN_RELOAD — magazine clack: bright HF click + bandpass burst
    { .model = GUN_MODEL_CLASSIC, .master_amp = 0.75,
      .click_amp = 0.85, .click_decay_ms = 0.4,
      .crack_amp = 0.90, .crack_decay_ms = 4.0,  .crack_fc = 4500, .crack_q = 3.0,
      .boom_amp  = 0.0,  .boom_freq_start = 0,   .boom_freq_end = 0,
      .boom_pitch_decay_ms = 1,  .boom_amp_decay_ms = 1,
      .tail_amp  = 0.20, .tail_attack_ms = 0,    .tail_decay_ms = 30,
      .tail_fc   = 2500, .tail_q = 0.6,
      .secondary_delay_ms = 80, .secondary_amp = 0.65,
      // Physical alt: tiny bore = sharp metallic transient + insert click
      .bore_length_s = 0.00010, .bore_loss = 0.70, .breech_reflect = 0.90,
      .pressure = 0.6, .env_rate = 4000.0, .noise_gain = 0.3,
      .body_freq = {2200, 4500, 8000}, .body_q = {10, 8, 6},
      .body_amp = {0.40, 0.30, 0.15}, .radiation = 0.92 },
    // --- GUN_COCK — bolt-action click-clack (two crisp clicks)
    { .model = GUN_MODEL_CLASSIC, .master_amp = 0.8,
      .click_amp = 0.90, .click_decay_ms = 0.4,
      .crack_amp = 1.00, .crack_decay_ms = 5.0,  .crack_fc = 3800, .crack_q = 3.2,
      .boom_amp  = 0.0,  .boom_freq_start = 0,   .boom_freq_end = 0,
      .boom_pitch_decay_ms = 1,  .boom_amp_decay_ms = 1,
      .tail_amp  = 0.15, .tail_attack_ms = 0,    .tail_decay_ms = 25,
      .tail_fc   = 2000, .tail_q = 0.6,
      .secondary_delay_ms = 55, .secondary_amp = 0.80,
      // Physical alt
      .bore_length_s = 0.00015, .bore_loss = 0.65, .breech_reflect = 0.88,
      .pressure = 0.7, .env_rate = 3500.0, .noise_gain = 0.35,
      .body_freq = {1800, 4200, 7500}, .body_q = {10, 8, 7},
      .body_amp = {0.45, 0.25, 0.15}, .radiation = 0.92 },
    // --- GUN_RICOCHET — pitched ping with downward pitch on release
    { .model = GUN_MODEL_CLASSIC, .master_amp = 0.85,
      .click_amp = 0.40, .click_decay_ms = 0.5,
      .crack_amp = 0.35, .crack_decay_ms = 7.0,  .crack_fc = 5500, .crack_q = 3.0,
      .boom_amp  = 0.95, .boom_freq_start = 1800,.boom_freq_end = 1500,
      .boom_pitch_decay_ms = 60, .boom_amp_decay_ms = 350,
      .tail_amp  = 0.20, .tail_attack_ms = 0,    .tail_decay_ms = 200,
      .tail_fc   = 3000, .tail_q = 1.0,
      // Physical alt: high-Q metallic ring (ricochet really IS that)
      .bore_length_s = 0.00040, .bore_loss = 0.15, .breech_reflect = 0.90,
      .pressure = 0.8, .env_rate = 600.0, .noise_gain = 0.3,
      .body_freq = {3000, 5500, 9000}, .body_q = {30, 25, 20},
      .body_amp = {0.40, 0.25, 0.15}, .radiation = 0.975 },
};

// ----- helper: precompute 2-pole resonant filter coefficients -----
//   y = b0*x + a1*y[n-1] - a2*y[n-2]
//   a1 = 2·r·cos(w),  a2 = r²,  r = exp(-π·f / (Q·sr)),  w = 2π·f/sr
// Output peak gain ≈ 1/(1-a1+a2) at DC and varies with Q. The b0 input
// gain is scaled so the resonant peak is approximately unity, making
// per-layer mix amps map to roughly equal loudness regardless of Q.
static inline void compute_resonator(double f, double q, double sr,
                                     double *a1, double *a2, double *b0) {
    if (q < 0.4) q = 0.4;
    if (f < 20.0) f = 20.0;
    if (f > sr * 0.45) f = sr * 0.45;
    double r = exp(-M_PI * f / (q * sr));
    double w = 2.0 * M_PI * f / sr;
    *a1 = 2.0 * r * cos(w);
    *a2 = r * r;
    // Peak gain of a 2-pole resonator ≈ 1/(1 - r). Pre-attenuate input
    // by that factor so the resonant peak stays near unity amplitude.
    *b0 = (1.0 - r);
}

// Initialize a voice's gun state from a preset. Called from audio_synth_gun.
// `force_model` overrides the preset's default model: -1 = preset default,
// 0 = CLASSIC, 1 = PHYSICAL. The preset table holds parameters for both
// models so the override always finds a populated config.
static void gun_init_voice(ACVoice *v, GunPreset preset, double sr,
                           int force_model) {
    if (preset < 0 || preset >= GUN_PRESET_COUNT) preset = GUN_PISTOL;
    const GunPresetParams *p = &gun_presets[preset];

    v->gun_preset = (int)preset;
    v->gun_model = (force_model == 0 || force_model == 1)
                   ? force_model : (int)p->model;
    v->gun_pressure = p->master_amp > 0.0 ? p->master_amp : 1.0;
    v->gun_pressure_env = 1.0;  // crack envelope (or physical excitation)
    v->gun_secondary_trig = p->secondary_delay_ms > 0
                            ? p->secondary_delay_ms * 0.001 * sr : 0.0;
    v->gun_secondary_amp = p->secondary_amp;
    v->gun_sustain_fire = p->sustain_fire;
    v->gun_retrig_timer = 0.0;
    v->gun_retrig_period = p->retrig_period_ms * 0.001;

    // Pitch sweep: nominal 1.0 at trigger. Ricochet sets target<1.0 on
    // release so boom freq drops (doppler-style).
    v->gun_pitch_mult = 1.0;
    v->gun_pitch_target = 1.0;
    v->gun_pitch_slew = 1.0 / (0.3 * sr);

    if (v->gun_model == GUN_MODEL_CLASSIC) {
        // Crack: exp decay multiplier from time-constant tau (in ms).
        double tau_crack = (p->crack_decay_ms > 0.1 ? p->crack_decay_ms : 0.1) * 0.001;
        v->gun_env_decay_mult = exp(-1.0 / (tau_crack * sr));

        // Boom: pitch sweep from start→end via geometric approach.
        // After tau seconds, distance to target is ~e^{-1} of original.
        v->gun_boom_freq_start = p->boom_freq_start;
        v->gun_boom_freq_end = p->boom_freq_end;
        v->gun_boom_freq = p->boom_freq_start;
        v->gun_boom_phase = 0.0;
        double tau_pitch = (p->boom_pitch_decay_ms > 0.1 ? p->boom_pitch_decay_ms : 0.1) * 0.001;
        v->gun_boom_pitch_mult = exp(-1.0 / (tau_pitch * sr));
        double tau_boom = (p->boom_amp_decay_ms > 0.1 ? p->boom_amp_decay_ms : 0.1) * 0.001;
        v->gun_boom_decay_mult = exp(-1.0 / (tau_boom * sr));
        v->gun_boom_env = (p->boom_amp > 0.0) ? 1.0 : 0.0;

        // Tail: linear attack ramp + exp decay.
        v->gun_tail_env = (p->tail_attack_ms > 0.0) ? 0.0 : 1.0;
        if (p->tail_attack_ms > 0.0) {
            v->gun_tail_attack_inc = 1.0 / (p->tail_attack_ms * 0.001 * sr);
        } else {
            v->gun_tail_attack_inc = 0.0;
        }
        double tau_tail = (p->tail_decay_ms > 0.1 ? p->tail_decay_ms : 0.1) * 0.001;
        v->gun_tail_decay_mult = exp(-1.0 / (tau_tail * sr));

        // Filter coeffs: body slot [0] = crack BPF, [1] = tail LPF.
        compute_resonator(p->crack_fc, p->crack_q, sr,
                          &v->gun_body_a1[0], &v->gun_body_a2[0], &v->gun_crack_b0);
        compute_resonator(p->tail_fc, p->tail_q, sr,
                          &v->gun_body_a1[1], &v->gun_body_a2[1], &v->gun_tail_b0);
        v->gun_tail_b1 = 0.0;
        v->gun_tail_b2 = 0.0;
        v->gun_body_y1[0] = v->gun_body_y2[0] = 0.0;
        v->gun_body_y1[1] = v->gun_body_y2[1] = 0.0;
        v->gun_body_y1[2] = v->gun_body_y2[2] = 0.0;
        // Layer mix gains.
        v->gun_body_amp[0] = p->crack_amp;
        v->gun_body_amp[1] = p->boom_amp;
        v->gun_body_amp[2] = p->tail_amp;
        // Click layer (sub-ms HF transient — adds the "tk" snap).
        v->gun_click_amp = p->click_amp;
        v->gun_click_env = (p->click_amp > 0.0) ? 1.0 : 0.0;
        v->gun_click_prev = 0.0;
        double tau_click = (p->click_decay_ms > 0.05 ? p->click_decay_ms : 0.05) * 0.001;
        v->gun_click_decay_mult = exp(-1.0 / (tau_click * sr));
        // Physical-only fields zeroed for safety.
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
        // PHYSICAL model — DWG bore + body modes.
        v->gun_bore_delay = p->bore_length_s * sr;
        if (v->gun_bore_delay < 4.0) v->gun_bore_delay = 4.0;
        if (v->gun_bore_delay > 2040.0) v->gun_bore_delay = 2040.0;
        v->gun_bore_loss = p->bore_loss;
        v->gun_bore_lp = 0.0;
        v->gun_breech_reflect = p->breech_reflect;
        v->gun_pressure = p->pressure;  // physical uses its own pressure scale
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
        // Classic-only fields zeroed.
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

// Called when a gun voice enters VOICE_KILLING — sets up release-time
// behaviors (ricochet pitch drop applies to both models via gun_pitch_mult).
static inline void gun_on_release(ACVoice *v) {
    if (v->type != WAVE_GUN) return;
    if (v->gun_preset == GUN_RICOCHET) {
        // Drop pitch on release — for classic this scales boom freq down;
        // for physical it stretches the bore delay (doppler).
        v->gun_pitch_target = (v->gun_model == GUN_MODEL_CLASSIC) ? 0.35 : 2.8;
    }
}

// Three-layer kick/snare-style gunshot synthesis. Output is summed
// crack (BPF noise) + boom (pitched osc with downward sweep) + tail
// (LPF noise with attack-decay), then scaled by master amp and the
// piece-supplied envelope.
static inline double generate_gun_classic_sample(ACVoice *v, double sr) {
    // --- Secondary trigger (rifle N-wave / 2nd click of cock/reload) ---
    if (v->gun_secondary_trig > 0.0) {
        v->gun_secondary_trig -= 1.0;
        if (v->gun_secondary_trig <= 0.0) {
            v->gun_pressure_env = v->gun_secondary_amp;     // refire crack
            v->gun_boom_env = v->gun_secondary_amp * 0.6;   // gentler boom
            v->gun_click_env = v->gun_secondary_amp;        // refire click too
            v->gun_secondary_trig = 0.0;
        }
    }

    // --- LMG sustain-fire retrigger ---
    if (v->gun_sustain_fire && v->state == VOICE_ACTIVE
        && isinf(v->duration) && v->gun_retrig_period > 0.0) {
        v->gun_retrig_timer += 1.0 / sr;
        if (v->gun_retrig_timer >= v->gun_retrig_period) {
            v->gun_retrig_timer -= v->gun_retrig_period;
            double j = (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
            double jitter = 0.82 + j * 0.32;  // ±18%
            v->gun_pressure_env = jitter;
            v->gun_boom_env = jitter;
            v->gun_click_env = jitter;
            v->gun_boom_freq = v->gun_boom_freq_start;  // restart pitch sweep
            // Tail keeps decaying (no re-attack) so rapid-fire feels continuous.
        }
    }

    // --- Pitch sweep (ricochet release doppler) ---
    if (v->gun_pitch_mult != v->gun_pitch_target) {
        v->gun_pitch_mult += (v->gun_pitch_target - v->gun_pitch_mult) * 0.00012;
    }

    // === Layer 0: CLICK — sub-millisecond HF transient ===
    // 1-zero HPF on white noise (y = x - x[n-1]) emphasizes the highest
    // frequencies. Combined with a ~0.5ms tau exp envelope it reads as
    // the sharp "tk" attack you expect at the front of a gunshot —
    // without it, the BPF crack on its own sounds like a shaped hiss.
    double click = 0.0;
    if (v->gun_click_env > 0.00002 && v->gun_click_amp > 0.0) {
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double hp = white - v->gun_click_prev;
        v->gun_click_prev = white;
        click = hp * v->gun_click_env * v->gun_click_amp;
        v->gun_click_env *= v->gun_click_decay_mult;
    }

    // === Layer 1: CRACK — bandpass-filtered noise burst ===
    double crack = 0.0;
    if (v->gun_pressure_env > 0.00002 && v->gun_body_amp[0] > 0.0) {
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        // 2-pole resonator (bandpass-like) on white noise.
        double y = v->gun_crack_b0 * white
                   + v->gun_body_a1[0] * v->gun_body_y1[0]
                   - v->gun_body_a2[0] * v->gun_body_y2[0];
        v->gun_body_y2[0] = v->gun_body_y1[0];
        v->gun_body_y1[0] = y;
        crack = y * v->gun_pressure_env * v->gun_body_amp[0];
        v->gun_pressure_env *= v->gun_env_decay_mult;
    }

    // === Layer 2: BOOM — pitched triangle with exponential pitch drop ===
    double boom = 0.0;
    if (v->gun_boom_env > 0.00002 && v->gun_body_amp[1] > 0.0) {
        // Geometric approach toward end freq. For typical 14ms tau at
        // 192kHz, this glides 250→55 Hz audibly within ~50ms.
        v->gun_boom_freq = v->gun_boom_freq_end
                          + (v->gun_boom_freq - v->gun_boom_freq_end) * v->gun_boom_pitch_mult;
        double f = v->gun_boom_freq * v->gun_pitch_mult;
        if (f < 1.0) f = 1.0;
        v->gun_boom_phase += f / sr;
        if (v->gun_boom_phase >= 1.0) v->gun_boom_phase -= 1.0;
        if (v->gun_boom_phase < 0.0) v->gun_boom_phase += 1.0;
        // Triangle wave — fatter low-end punch than sine, less harsh than square.
        double tp = v->gun_boom_phase;
        double s = (tp < 0.5) ? (4.0 * tp - 1.0) : (3.0 - 4.0 * tp);
        boom = s * v->gun_boom_env * v->gun_body_amp[1];
        v->gun_boom_env *= v->gun_boom_decay_mult;
    }

    // === Layer 3: TAIL — lowpass-filtered noise rumble ===
    double tail = 0.0;
    if (v->gun_body_amp[2] > 0.0) {
        // Envelope: linear ramp during attack phase, then exp decay.
        if (v->gun_tail_attack_inc > 0.0) {
            v->gun_tail_env += v->gun_tail_attack_inc;
            if (v->gun_tail_env >= 1.0) {
                v->gun_tail_env = 1.0;
                v->gun_tail_attack_inc = 0.0;  // attack done; switch to decay
            }
        } else if (v->gun_tail_env > 0.00001) {
            v->gun_tail_env *= v->gun_tail_decay_mult;
        }
        if (v->gun_tail_env > 0.00001) {
            double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
            // 2-pole resonator at low freq, low Q ≈ 1-pole-ish lowpass behavior.
            double y = v->gun_tail_b0 * white
                       + v->gun_body_a1[1] * v->gun_body_y1[1]
                       - v->gun_body_a2[1] * v->gun_body_y2[1];
            v->gun_body_y2[1] = v->gun_body_y1[1];
            v->gun_body_y1[1] = y;
            tail = y * v->gun_tail_env * v->gun_body_amp[2];
        }
    }

    // Click also retriggers on secondary/sustain events because gun_click_env
    // gets reset in those branches via gun_pressure_env (no — actually it
    // doesn't; we only reset crack/boom there). Fold a small click retrigger
    // into the secondary path so reload+cock 2nd hits feel just as crisp.
    double out = (click + crack + boom + tail) * v->gun_pressure;
    return out * compute_envelope(v);
}

// Physical-model gunshot — DWG bore + parallel body-mode resonators +
// muzzle radiation HPF. Better for cavity-dominated weapons (grenade,
// RPG launch tube) where the bore length is meaningful.
static inline double generate_gun_physical_sample(ACVoice *v, double sr) {
    // Excitation: short-lived pressure pulse modulated by turbulent noise.
    double excite = 0.0;
    if (v->gun_pressure_env > 0.00002) {
        uint32_t n = xorshift32(&v->noise_seed);
        double white = ((double)n / (double)UINT32_MAX) * 2.0 - 1.0;
        excite = v->gun_pressure_env * v->gun_pressure
                 * (1.0 + v->gun_noise_gain * white);
        v->gun_pressure_env *= v->gun_env_decay_mult;
    }

    // Secondary trigger (RPG delayed explosion).
    if (v->gun_secondary_trig > 0.0) {
        v->gun_secondary_trig -= 1.0;
        if (v->gun_secondary_trig <= 0.0) {
            v->gun_pressure_env = v->gun_secondary_amp;
            v->gun_secondary_trig = 0.0;
        }
    }

    // Sustain fire (not used by current physical presets but supported).
    if (v->gun_sustain_fire && v->state == VOICE_ACTIVE
        && isinf(v->duration) && v->gun_retrig_period > 0.0) {
        v->gun_retrig_timer += 1.0 / sr;
        if (v->gun_retrig_timer >= v->gun_retrig_period) {
            v->gun_retrig_timer -= v->gun_retrig_period;
            v->gun_pressure_env = 1.0;
            double j = (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
            v->gun_pressure_env *= 0.82 + j * 0.32;
        }
    }

    // Pitch sweep approach (ricochet — currently classic-only, kept for parity).
    if (v->gun_pitch_mult != v->gun_pitch_target) {
        v->gun_pitch_mult += (v->gun_pitch_target - v->gun_pitch_mult) * 0.00012;
    }
    double bore_delay = v->gun_bore_delay * v->gun_pitch_mult;
    if (bore_delay < 4.0) bore_delay = 4.0;
    if (bore_delay > 2040.0) bore_delay = 2040.0;

    // Bore delay loop: closed breech (+refl) / open muzzle (−refl + LPF damping).
    const int BORE_N = 2048;
    double bore_out = whistle_frac_read(v->whistle_bore_buf, BORE_N,
                                        v->whistle_bore_w, bore_delay);
    v->gun_bore_lp = v->gun_bore_loss * (-bore_out)
                     + (1.0 - v->gun_bore_loss) * v->gun_bore_lp;
    double refl = v->gun_bore_lp;
    double into_bore = excite + refl * v->gun_breech_reflect;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)into_bore;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % BORE_N;

    // Muzzle radiation: 1-zero HPF (open end radiates highs preferentially).
    double radiated = into_bore - v->gun_radiation_a * v->gun_rad_prev;
    v->gun_rad_prev = into_bore;

    // Body modes — parallel pole-pair resonators driven by excitation.
    // Lowered Q values in presets keep these from ringing metallically.
    double body = 0.0;
    for (int i = 0; i < 3; i++) {
        double y = excite
                   + v->gun_body_a1[i] * v->gun_body_y1[i]
                   - v->gun_body_a2[i] * v->gun_body_y2[i];
        v->gun_body_y2[i] = v->gun_body_y1[i];
        v->gun_body_y1[i] = y;
        body += y * v->gun_body_amp[i];
    }

    double out = radiated * 0.55 + body * 0.45;
    return out * compute_envelope(v);
}

static inline double generate_gun_sample(ACVoice *v, double sr) {
    if (v->gun_model == GUN_MODEL_PHYSICAL) {
        return generate_gun_physical_sample(v, sr);
    }
    return generate_gun_classic_sample(v, sr);
}

static inline double compute_fade(ACVoice *v) {
    if (v->state != VOICE_KILLING) return 1.0;
    if (v->fade_duration <= 0.0) return 0.0;
    double progress = v->fade_elapsed / v->fade_duration;
    if (progress >= 1.0) return 0.0;
    return 1.0 - progress;
}

static inline double generate_sample(ACVoice *v, double sample_rate) {
    double s;
    switch (v->type) {
    case WAVE_SINE:
        s = sin(2.0 * M_PI * v->phase);
        break;
    case WAVE_SQUARE:
        s = v->phase < 0.5 ? 1.0 : -1.0;
        break;
    case WAVE_TRIANGLE: {
        // Offset phase by 0.25 to start at zero crossing (matches synth.mjs)
        double tp = v->phase + 0.25;
        if (tp >= 1.0) tp -= 1.0;
        s = 4.0 * fabs(tp - 0.5) - 1.0;
        break;
    }
    case WAVE_SAWTOOTH:
        s = 2.0 * v->phase - 1.0;
        break;
    case WAVE_NOISE: {
        // Filtered white noise using biquad LPF
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double y = v->noise_b0 * white + v->noise_b1 * v->noise_x1 + v->noise_b2 * v->noise_x2
                   - v->noise_a1 * v->noise_y1 - v->noise_a2 * v->noise_y2;
        v->noise_x2 = v->noise_x1;
        v->noise_x1 = white;
        v->noise_y2 = v->noise_y1;
        v->noise_y1 = y;
        s = y;
        break;
    }
    case WAVE_WHISTLE:
        s = generate_whistle_sample(v, sample_rate);
        break;
    case WAVE_GUN:
        s = generate_gun_sample(v, sample_rate);
        break;
    default:
        s = 0.0;
    }

    // Smooth frequency toward target (uses precomputed alpha from caller)
    if (v->target_frequency > 0 && v->frequency != v->target_frequency) {
        v->frequency += (v->target_frequency - v->frequency) * 0.0003; // ~5ms at 192kHz
    }

    // Advance phase for basic oscillators; whistle/gun use their own DWG state.
    if (v->type != WAVE_WHISTLE && v->type != WAVE_GUN) {
        v->phase += v->frequency / sample_rate;
        if (v->phase >= 1.0) v->phase -= 1.0;
    }

    return s;
}

// Setup biquad LPF coefficients for noise voice
static void setup_noise_filter(ACVoice *v, double sample_rate) {
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

// ============================================================
// Audio thread
// ============================================================

#define ROOM_DELAY_SAMPLES (int)(0.12 * AUDIO_SAMPLE_RATE) // 120ms
#define ROOM_SIZE (ROOM_DELAY_SAMPLES * 3)
#define ROOM_FEEDBACK 0.3
#define ROOM_MIX 0.35

// Soft clamp (tanh-style) to prevent harsh digital clipping
// Smooth curve: starts compressing gently above 0.6, hard-limits at ~0.95
static inline double soft_clip(double x) {
    if (x > 0.6) {
        double over = x - 0.6;
        return 0.6 + 0.35 * (1.0 - 1.0 / (1.0 + over * 2.5));
    }
    if (x < -0.6) {
        double over = -x - 0.6;
        return -0.6 - 0.35 * (1.0 - 1.0 / (1.0 + over * 2.5));
    }
    return x;
}

// Compressor state (per-channel peak follower)
static double comp_env = 0.0;  // envelope follower level
static unsigned long xrun_count = 0;
static unsigned long short_write_count = 0;

static void mix_sample_voice(SampleVoice *sv, const float *buf, int slen, int smax,
                             double rate, double *mix_l, double *mix_r) {
    if (!sv || !sv->active || !buf || slen <= 0 || smax <= 0) {
        if (sv) sv->active = 0;
        return;
    }

    if (slen > smax) slen = smax;

    // Fade envelope (5ms attack/release at output rate)
    double fade_speed = 1.0 / (0.005 * rate);
    if (sv->fade < sv->fade_target) {
        sv->fade += fade_speed;
        if (sv->fade > sv->fade_target) sv->fade = sv->fade_target;
    } else if (sv->fade > sv->fade_target) {
        sv->fade -= fade_speed;
        if (sv->fade <= 0.0) { sv->fade = 0.0; sv->active = 0; return; }
    }

    // Pan controls both amplitude and a small Haas-style stereo offset.
    double delay_samps = sv->pan * 0.0004 * rate;
    double pos_l = sv->position - (delay_samps > 0 ? delay_samps : 0);
    double pos_r = sv->position + (delay_samps > 0 ? 0 : delay_samps);
    if (pos_l < 0) pos_l = 0;
    if (pos_r < 0) pos_r = 0;

    int p0l = (int)pos_l;
    if (sv->loop) {
        p0l = ((p0l % slen) + slen) % slen;
    } else if (p0l >= slen) {
        sv->active = 0;
        return;
    }
    int p1l = p0l + 1;
    if (p1l >= slen) p1l = sv->loop ? 0 : p0l;
    if (p0l >= smax || p1l >= smax) { sv->active = 0; return; }
    double fl = pos_l - p0l;
    double samp_l = buf[p0l] * (1.0 - fl) + buf[p1l] * fl;

    int p0r = (int)pos_r;
    if (sv->loop) {
        p0r = ((p0r % slen) + slen) % slen;
    } else if (p0r >= slen) {
        p0r = slen - 1;
    }
    if (p0r < 0) p0r = 0;
    int p1r = p0r + 1;
    if (p1r >= slen) p1r = sv->loop ? 0 : p0r;
    if (p0r >= smax || p1r >= smax) { sv->active = 0; return; }
    double fr = pos_r - p0r;
    double samp_r = buf[p0r] * (1.0 - fr) + buf[p1r] * fr;

    double vol = sv->volume * sv->fade;
    double l_gain = sv->pan <= 0 ? 1.0 : 1.0 - sv->pan * 0.6;
    double r_gain = sv->pan >= 0 ? 1.0 : 1.0 + sv->pan * 0.6;
    *mix_l += samp_l * vol * l_gain;
    *mix_r += samp_r * vol * r_gain;

    sv->position += sv->speed;
    if (sv->position >= slen) {
        if (sv->loop) {
            while (sv->position >= slen) sv->position -= slen;
        } else {
            sv->active = 0;
        }
    } else if (sv->position < 0.0) {
        if (sv->loop) {
            while (sv->position < 0.0) sv->position += slen;
        } else {
            sv->active = 0;
        }
    }
}

static void *audio_thread_fn(void *arg) {
    ACAudio *audio = (ACAudio *)arg;
    const unsigned int period_frames = audio->actual_period ? audio->actual_period : AUDIO_PERIOD_SIZE;
    int16_t *buffer = calloc(period_frames * AUDIO_CHANNELS, sizeof(int16_t));
    if (!buffer) { fprintf(stderr, "[audio] thread: alloc failed\n"); return NULL; }
    const double rate = (double)(audio->actual_rate ? audio->actual_rate : AUDIO_SAMPLE_RATE);
    const double dt = 1.0 / rate;
    double mix_divisor = 1.0; // Smooth auto-mix (matches speaker.mjs)
    // Auto-mix smoothing: fast-ish attack, slower release to avoid zipper clicks.
    const double mix_att_coeff = 1.0 - exp(-1.0 / (0.004 * rate)); // ~4ms
    const double mix_rel_coeff = 1.0 - exp(-1.0 / (0.060 * rate)); // ~60ms

    // Drum bus peak compressor — gives percussion proper "stacking" feel.
    // The drum bus sums additively (no auto-mix divide) so rapid hits
    // would otherwise saturate through soft_clip tanh, flattening peaks
    // and making each new hit sound QUIETER. A real peak compressor
    // with fast attack / slower release keeps the drum bus below ~0.95
    // so transients retain impact AND the compressor recovers between
    // hits so each kick/snare feels punchy on its own.
    double drum_gain = 1.0;
    const double DRUM_THRESH = 0.95;
    // 5ms attack — slower than a 2ms beater transient so the first peak
    // of each hit passes through at full amplitude before compression
    // engages. This preserves the "snap" of each individual kick/snare.
    const double drum_att_coeff = 1.0 - exp(-1.0 / (0.005 * rate));
    // 200ms release — recovers quickly enough that successive hits at
    // typical tempos (120-200 BPM, 300-500ms between hits) each get
    // the benefit of full dynamic range.
    const double drum_rel_coeff = 1.0 - exp(-1.0 / (0.200 * rate));

    // Set real-time priority to prevent audio glitches from background tasks
    struct sched_param sp = { .sched_priority = 50 };
    if (pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp) != 0)
        fprintf(stderr, "[audio] Warning: couldn't set RT priority\n");

    while (audio->running) {
        memset(buffer, 0, sizeof(buffer));

        pthread_mutex_lock(&audio->lock);

        for (unsigned int i = 0; i < period_frames; i++) {
            // Split the voice bus in two: TONES get auto-mix normalization
            // (divide by total voice weight so held chords stay balanced),
            // DRUMS stack additively (so a kick+snare+hat transient sums to
            // a louder peak instead of ducking itself). soft_clip at the end
            // catches any drum peak excess with tanh saturation — which
            // gives percussion a natural analog "push" character.
            //
            // Heuristic: a voice is percussive if it has a SHORT FINITE
            // duration (< 0.5s). Held tones (duration = Infinity) and
            // long one-shot tones always go through the auto-mix bus.
            double tone_l = 0.0, tone_r = 0.0;
            double drum_l = 0.0, drum_r = 0.0;
            double voice_sum = 0.0; // Tone-only voice weight for auto-mix

            for (int v = 0; v < AUDIO_MAX_VOICES; v++) {
                ACVoice *voice = &audio->voices[v];
                if (voice->state == VOICE_INACTIVE) continue;

                double s = generate_sample(voice, rate);
                double env = compute_envelope(voice);
                double fade = compute_fade(voice);
                double amp = s * env * fade * voice->volume;

                double left_gain = (1.0 - voice->pan) * 0.5;
                double right_gain = (1.0 + voice->pan) * 0.5;

                int is_percussive = !isinf(voice->duration) && voice->duration < 0.5;
                if (is_percussive) {
                    // Drum bus — no auto-mix normalization. Drums stack
                    // additively and rely on soft_clip for peak control.
                    drum_l += amp * left_gain;
                    drum_r += amp * right_gain;
                } else {
                    // Tone bus — contributes to voice_sum for auto-mix.
                    tone_l += amp * left_gain;
                    tone_r += amp * right_gain;
                    if (voice->state == VOICE_KILLING) {
                        voice_sum += voice->volume * (1.0 - voice->fade_elapsed / voice->fade_duration);
                    } else {
                        voice_sum += voice->volume;
                    }
                }

                voice->elapsed += dt;
                if (voice->state == VOICE_KILLING) {
                    voice->fade_elapsed += dt;
                    if (voice->fade_elapsed >= voice->fade_duration)
                        voice->state = VOICE_INACTIVE;
                } else if (!isinf(voice->duration) && voice->elapsed >= voice->duration) {
                    voice->state = VOICE_INACTIVE;
                }
            }

            // Smooth auto-mix divisor — fast attack, slow release.
            // Applied ONLY to the tone bus. Drums bypass it entirely.
            double target = voice_sum > 1.0 ? voice_sum : 1.0;
            if (mix_divisor < target)
                mix_divisor += (target - mix_divisor) * mix_att_coeff;
            else if (mix_divisor > target)
                mix_divisor += (target - mix_divisor) * mix_rel_coeff;
            if (mix_divisor < 1.0) mix_divisor = 1.0;

            tone_l /= mix_divisor;
            tone_r /= mix_divisor;

            // Drum bus peak compressor: detect peak, attack fast if over
            // threshold, release slow. Unlike the tone auto-mix divide,
            // this preserves individual hit dynamics — a single drum hit
            // passes through at full amplitude, but sustained buildup
            // from overlapping hits gets gain-reduced gracefully so they
            // stack linearly instead of saturating through soft_clip.
            {
                double peak = fabs(drum_l);
                double peak_r = fabs(drum_r);
                if (peak_r > peak) peak = peak_r;
                double target = (peak > DRUM_THRESH) ? (DRUM_THRESH / peak) : 1.0;
                if (target < drum_gain) {
                    drum_gain += (target - drum_gain) * drum_att_coeff;
                } else {
                    drum_gain += (target - drum_gain) * drum_rel_coeff;
                }
                drum_l *= drum_gain;
                drum_r *= drum_gain;
            }

            // Merge the two buses. Drums land compressed to ~0.95 peak
            // so they retain impact without saturating the final output.
            double mix_l = tone_l + drum_l;
            double mix_r = tone_r + drum_r;

            // Mix sample voices (pitch-shifted playback)
            // Lock already held from line 246 — safe to read sample_buf
            for (int v = 0; v < AUDIO_MAX_SAMPLE_VOICES; v++) {
                SampleVoice *sv = &audio->sample_voices[v];
                mix_sample_voice(sv, audio->sample_buf, audio->sample_len, audio->sample_max_len,
                                 rate, &mix_l, &mix_r);
            }

            // Dedicated global replay voice. Uses its own buffer so reverse
            // playback can overlap normal sample-bank activity.
            mix_sample_voice(&audio->replay_voice, audio->replay_buf,
                             audio->replay_len, audio->replay_max_len,
                             rate, &mix_l, &mix_r);
            // (lock released at end of buffer loop)

            // Mix DJ deck audio (lock-free: single consumer = audio thread)
            // Speed control: advance ring read by `speed` samples per output sample
            // with linear interpolation for smooth pitch shifting / scratching.
            for (int d = 0; d < AUDIO_MAX_DECKS; d++) {
                ACDeck *dk = &audio->decks[d];
                if (!dk->active || !dk->playing || !dk->decoder) continue;
                ACDeckDecoder *dec = dk->decoder;
                double spd = dec->speed;
                if (spd < -4.0) spd = -4.0;
                if (spd > 4.0) spd = 4.0;
                int64_t avail = dec->ring_write - dec->ring_read;
                if (avail <= 1) continue;
                // Fractional ring position for interpolation
                double frac_pos = dec->ring_frac;
                int64_t base = dec->ring_read;
                int64_t idx0 = base + (int64_t)frac_pos;
                if (idx0 < base || idx0 + 1 >= dec->ring_write) {
                    // Not enough data — skip
                    continue;
                }
                double t = frac_pos - (int64_t)frac_pos;
                int ri0 = (idx0 % dec->ring_size) * 2;
                int ri1 = ((idx0 + 1) % dec->ring_size) * 2;
                float sl = dec->ring[ri0]     * (1.0f - (float)t) + dec->ring[ri1]     * (float)t;
                float sr = dec->ring[ri0 + 1] * (1.0f - (float)t) + dec->ring[ri1 + 1] * (float)t;
                // Advance fractional position by speed
                dec->ring_frac += spd;
                // Consume whole samples from ring
                int consumed = (int)dec->ring_frac;
                if (consumed > 0) {
                    dec->ring_read += consumed;
                    dec->ring_frac -= consumed;
                } else if (consumed < 0) {
                    // Reverse: clamp to not go before ring_read
                    // (reverse scratching won't replay old audio, just stops)
                    dec->ring_frac = 0;
                }
                // Crossfader: 0.0 = full deck A, 1.0 = full deck B
                float cf = (d == 0)
                    ? (1.0f - audio->crossfader)
                    : audio->crossfader;
                float vol = dk->volume * cf * audio->deck_master_volume;
                mix_l += sl * vol;
                mix_r += sr * vol;
                // Wake decoder thread if ring drained below 50%
                if ((dec->ring_write - dec->ring_read) < dec->ring_size / 2) {
                    pthread_mutex_lock(&dec->mutex);
                    pthread_cond_signal(&dec->cond);
                    pthread_mutex_unlock(&dec->mutex);
                }
            }

            // Smooth room_mix toward target (~10ms at 192kHz)
            if (audio->room_mix != audio->target_room_mix) {
                audio->room_mix += (audio->target_room_mix - audio->room_mix) * 0.00005f;
            }

            // Smooth fx_mix toward target
            if (audio->fx_mix != audio->target_fx_mix) {
                audio->fx_mix += (audio->target_fx_mix - audio->fx_mix) * 0.00005f;
            }

            // Smooth bitcrush mix toward target
            if (audio->glitch_mix != audio->target_glitch_mix) {
                audio->glitch_mix += (audio->target_glitch_mix - audio->glitch_mix) * 0.00005f;
            }

            // Save dry signal before FX chain
            double dry_l = mix_l, dry_r = mix_r;

            // Capture recent dry output for true reverse replay. This stores
            // the actual mixed audio (not note events) before room/glitch/TTS
            // so the reverse replay can run back through the live FX chain.
            if (audio->output_history_buf && audio->output_history_size > 0) {
                unsigned int stride = audio->output_history_downsample_n;
                if (stride == 0) stride = 1;
                audio->output_history_downsample_pos++;
                if (audio->output_history_downsample_pos >= stride) {
                    audio->output_history_downsample_pos = 0;
                    uint64_t wp = audio->output_history_write_pos;
                    audio->output_history_buf[wp % (uint64_t)audio->output_history_size] =
                        (float)((dry_l + dry_r) * 0.5);
                    audio->output_history_write_pos = wp + 1;
                }
            }

            // Room (reverb) effect — tap delays based on actual sample rate
            if (audio->room_enabled && audio->room_buf_l) {
                float rmix = audio->room_mix;
                int rs = audio->room_size;

                // At 0% mix, skip all reverb processing (no buffer feed, no output)
                if (rmix > 0.001f) {
                    int room_delay = (int)(0.12 * rate); // 120ms in samples
                    int tap1 = (audio->room_pos - room_delay + rs) % rs;
                    int tap2 = (audio->room_pos - room_delay * 2 + rs) % rs;
                    int tap3 = (audio->room_pos - room_delay * 3 + rs) % rs;

                    // Weighted sum of taps, normalized
                    float wet_l = (audio->room_buf_l[tap1] * 0.5f
                                 + audio->room_buf_l[tap2] * 0.3f
                                 + audio->room_buf_l[tap3] * 0.2f);
                    float wet_r = (audio->room_buf_r[tap1] * 0.5f
                                 + audio->room_buf_r[tap2] * 0.3f
                                 + audio->room_buf_r[tap3] * 0.2f);

                    // Feed buffer: dry input + attenuated wet feedback
                    float fb_l = (float)mix_l + wet_l * ROOM_FEEDBACK;
                    float fb_r = (float)mix_r + wet_r * ROOM_FEEDBACK;
                    // Damping — ensures reverb tail always decays
                    fb_l *= 0.995f;
                    fb_r *= 0.995f;
                    // Soft-limit feedback to avoid hard-clamp discontinuities under transients.
                    fb_l = tanhf(fb_l * 0.65f) / 0.65f;
                    fb_r = tanhf(fb_r * 0.65f) / 0.65f;
                    audio->room_buf_l[audio->room_pos] = fb_l;
                    audio->room_buf_r[audio->room_pos] = fb_r;

                    // Mix wet into output
                    mix_l = mix_l * (1.0 - rmix) + wet_l * rmix;
                    mix_r = mix_r * (1.0 - rmix) + wet_r * rmix;
                } else {
                    // Mix is ~0%: just clear the current buffer position (drain residue)
                    audio->room_buf_l[audio->room_pos] = 0.0f;
                    audio->room_buf_r[audio->room_pos] = 0.0f;
                }
                audio->room_pos = (audio->room_pos + 1) % rs;
            }

            // Glitch (sample-hold + bitcrush)
            // `glitch_mix` scales the intensity of the stage itself, while
            // `fx_mix` still controls the dry/wet blend of the whole FX chain.
            {
                float gmix = audio->glitch_mix;
                if (gmix > 0.001f) {
                    float crush = gmix * gmix;
                    int hold_interval = 1 + (int)roundf((float)(audio->glitch_rate - 1) * crush);
                    int bits = 12 - (int)roundf(gmix * 8.0f); // 12-bit -> 4-bit
                    if (bits < 4) bits = 4;
                    if (bits > 12) bits = 12;
                    int levels = 1 << bits;

                    audio->glitch_counter++;
                    if (audio->glitch_counter >= hold_interval) {
                        audio->glitch_counter = 0;
                        audio->glitch_hold_l = roundf((float)mix_l * levels) / levels;
                        audio->glitch_hold_r = roundf((float)mix_r * levels) / levels;
                    }

                    mix_l = mix_l * (1.0f - gmix) + audio->glitch_hold_l * gmix;
                    mix_r = mix_r * (1.0f - gmix) + audio->glitch_hold_r * gmix;
                }
            }

            // Blend dry/wet based on FX mix
            {
                float fxm = audio->fx_mix;
                if (fxm < 0.999f) {
                    mix_l = dry_l * (1.0 - fxm) + mix_l * fxm;
                    mix_r = dry_r * (1.0 - fxm) + mix_r * fxm;
                }
            }

            // Mix in TTS audio after FX chain (bypasses reverb/glitch)
            // Fade envelope prevents hard-start/stop clicks
            {
                int tts_has_data = audio->tts_buf &&
                    (audio->tts_read_pos != audio->tts_write_pos);
                // ~3ms ramp at 192kHz (1/576 per sample)
                float ramp = 1.0f / 576.0f;
                if (tts_has_data) {
                    audio->tts_fade += ramp;
                    if (audio->tts_fade > 1.0f) audio->tts_fade = 1.0f;
                    float tts_sample = audio->tts_buf[audio->tts_read_pos]
                        * audio->tts_volume * audio->tts_fade;
                    mix_l += tts_sample;
                    mix_r += tts_sample;
                    audio->tts_read_pos = (audio->tts_read_pos + 1) % audio->tts_buf_size;
                } else {
                    // Fade out: keep adding the last scaled zero-ish sample
                    if (audio->tts_fade > 0.0f) {
                        audio->tts_fade -= ramp;
                        if (audio->tts_fade < 0.0f) audio->tts_fade = 0.0f;
                    }
                }
            }

            // Compressor: peak-following gain reduction (threshold 0.4, ratio ~8:1)
            {
                double peak = fabs(mix_l);
                double pr = fabs(mix_r);
                if (pr > peak) peak = pr;
                // Attack: very fast (0.2ms), Release: medium (40ms)
                double att_coeff = 1.0 - exp(-1.0 / (0.0002 * rate));
                double rel_coeff = 1.0 - exp(-1.0 / (0.04 * rate));
                if (peak > comp_env)
                    comp_env += att_coeff * (peak - comp_env);
                else
                    comp_env += rel_coeff * (peak - comp_env);
                if (comp_env > 0.4) {
                    double gain = 0.4 + (comp_env - 0.4) * 0.125; // ~8:1 ratio above threshold
                    double reduction = gain / comp_env;
                    mix_l *= reduction;
                    mix_r *= reduction;
                }
            }

            // Apply system volume (software gain — hardware mixer may not attenuate)
            {
                double vol = audio->system_volume * 0.01; // 0-100 → 0.0-1.0
                // Use squared curve for more natural volume perception
                vol = vol * vol;
                mix_l *= vol;
                mix_r *= vol;
            }

            // Soft clip and convert to int16
            mix_l = soft_clip(mix_l);
            mix_r = soft_clip(mix_r);

            buffer[i * 2] = (int16_t)(mix_l * 26000);
            buffer[i * 2 + 1] = (int16_t)(mix_r * 26000);

            // HDMI audio: 1-pole low-pass filter + downsample
            // (volume already applied above to mix_l/mix_r)
            if (audio->hdmi_pcm) {
                // LP filter (alpha ≈ 0.18 → ~3kHz cutoff at 48kHz)
                float alpha = 0.18f;
                audio->hdmi_lp_l = alpha * (float)mix_l + (1.0f - alpha) * audio->hdmi_lp_l;
                audio->hdmi_lp_r = alpha * (float)mix_r + (1.0f - alpha) * audio->hdmi_lp_r;
                // Downsample: one HDMI sample per N primary samples
                audio->hdmi_downsample_pos++;
                if (audio->hdmi_downsample_pos >= audio->hdmi_downsample_n) {
                    audio->hdmi_downsample_pos = 0;
                    int pp = audio->hdmi_period_pos;
                    if (pp + 1 < (int)(sizeof(audio->hdmi_period) / sizeof(int16_t)) / 2) {
                        audio->hdmi_period[pp * 2]     = (int16_t)(audio->hdmi_lp_l * 28000);
                        audio->hdmi_period[pp * 2 + 1] = (int16_t)(audio->hdmi_lp_r * 28000);
                        audio->hdmi_period_pos++;
                        if (audio->hdmi_period_pos >= audio->hdmi_period_size) {
                            snd_pcm_t *hpcm = (snd_pcm_t *)audio->hdmi_pcm;
                            int hw = snd_pcm_writei(hpcm, audio->hdmi_period,
                                                     audio->hdmi_period_size);
                            if (hw == -EPIPE || hw == -ESTRPIPE)
                                snd_pcm_recover(hpcm, hw, 1);
                            audio->hdmi_period_pos = 0;
                        }
                    }
                }
            }

            // Store waveform for visualization
            int wp = audio->waveform_pos;
            audio->waveform_left[wp] = (float)mix_l;
            audio->waveform_right[wp] = (float)mix_r;
            audio->waveform_pos = (wp + 1) % AUDIO_WAVEFORM_SIZE;

            // Track amplitude
            float al = fabsf((float)mix_l);
            float ar = fabsf((float)mix_r);
            audio->amplitude_left = audio->amplitude_left * 0.99f + al * 0.01f;
            audio->amplitude_right = audio->amplitude_right * 0.99f + ar * 0.01f;
        }

        // BPM metronome
        audio->beat_elapsed += (double)period_frames * dt;
        double beat_interval = 60.0 / audio->bpm;
        if (audio->beat_elapsed >= beat_interval) {
            audio->beat_elapsed -= beat_interval;
            audio->beat_triggered = 1;
        }

        pthread_mutex_unlock(&audio->lock);

        audio->total_frames += period_frames;
        audio->time = (double)audio->total_frames / rate;

        // Recording tap: send mixed PCM to recorder (if active). Used by
        // the MP4 tape recorder (recorder.c) for the audio track.
        if (audio->rec_callback)
            audio->rec_callback(buffer, period_frames, audio->rec_userdata);

        // Write to ALSA (handle short writes to avoid dropped samples/clicks)
        snd_pcm_t *pcm = (snd_pcm_t *)audio->pcm;
        int remaining = (int)period_frames;
        int offset = 0;
        while (remaining > 0) {
            int frames = snd_pcm_writei(pcm, buffer + offset * AUDIO_CHANNELS, remaining);
            if (frames == -EAGAIN) continue;
            if (frames < 0) {
                int rec = snd_pcm_recover(pcm, frames, 1);
                if (frames == -EPIPE || frames == -ESTRPIPE) {
                    xrun_count++;
                    if ((xrun_count % 32) == 1) {
                        fprintf(stderr, "[audio] XRUN recovered x%lu\n", xrun_count);
                    }
                }
                if (rec < 0) {
                    fprintf(stderr, "[audio] ALSA write failed: %s\n", snd_strerror(rec));
                    break;
                }
                continue;
            }
            if (frames == 0) continue;
            if (frames < remaining) {
                short_write_count++;
                if ((short_write_count % 64) == 1) {
                    fprintf(stderr, "[audio] Short write x%lu (%d/%d)\n",
                            short_write_count, frames, remaining);
                }
            }
            remaining -= frames;
            offset += frames;
        }
    }

    free(buffer);
    return NULL;
}

// ============================================================
// Public API
// ============================================================

// Seed a small default sample so sample mode is playable before first mic recording.
// This roughly matches the "startup" one-shot feel used in web notepat.
static void seed_default_sample(ACAudio *audio) {
    if (!audio || !audio->sample_buf || audio->sample_max_len <= 0) return;

    const unsigned int rate = 48000;
    int len = (int)(0.55 * (double)rate); // 550ms one-shot
    if (len > audio->sample_max_len) len = audio->sample_max_len;

    double p1 = 0.0, p2 = 0.0, p3 = 0.0;
    for (int i = 0; i < len; i++) {
        double t = (double)i / (double)rate;
        double env = exp(-6.0 * t) * (1.0 - exp(-35.0 * t)); // fast attack, exponential decay
        double f0 = 240.0 + 50.0 * sin(t * 6.0);             // slight wobble
        double f1 = f0 * 2.01;
        double f2 = f0 * 3.02;
        p1 += 2.0 * M_PI * f0 / (double)rate;
        p2 += 2.0 * M_PI * f1 / (double)rate;
        p3 += 2.0 * M_PI * f2 / (double)rate;

        double s = 0.78 * sin(p1) + 0.22 * sin(p2 + 0.25) + 0.08 * sin(p3 + 0.15);
        audio->sample_buf[i] = (float)(s * env * 0.85);
    }
    for (int i = len; i < audio->sample_max_len; i++) audio->sample_buf[i] = 0.0f;

    audio->sample_len = len;
    audio->sample_rate = rate;
    ac_log("[sample] seeded default startup sample (%d frames @ %u Hz)\n",
           audio->sample_len, audio->sample_rate);
}

ACAudio *audio_init(void) {
    ACAudio *audio = calloc(1, sizeof(ACAudio));
    if (!audio) return NULL;

    audio->bpm = 120.0;
    audio->actual_rate = AUDIO_SAMPLE_RATE; // default, overwritten after ALSA negotiation
    audio->glitch_rate = AUDIO_SAMPLE_RATE / 1600;
    pthread_mutex_init(&audio->lock, NULL);

    // Allocate reverb buffers
    audio->room_size = ROOM_SIZE;
    audio->room_mix = 0.0f;  // Start dry, trackpad Y controls
    audio->room_enabled = 1; // Always on, mix controls wet amount
    audio->glitch_mix = 0.0f;
    audio->target_glitch_mix = 0.0f;
    audio->fx_mix = 1.0f;    // FX chain fully wet by default
    audio->target_fx_mix = 1.0f;
    audio->room_buf_l = calloc(ROOM_SIZE, sizeof(float));
    audio->room_buf_r = calloc(ROOM_SIZE, sizeof(float));

    // Sample buffer (10 seconds at max 48kHz capture rate)
    audio->sample_max_len = 48000 * AUDIO_MAX_SAMPLE_SECS;
    audio->sample_buf = calloc(audio->sample_max_len, sizeof(float));
    audio->sample_buf_back = calloc(audio->sample_max_len, sizeof(float));
    audio->sample_len = 0;
    audio->sample_rate = 48000; // default, overwritten by actual capture rate
    audio->sample_next_id = 1;
    audio->replay_max_len = AUDIO_OUTPUT_HISTORY_RATE * AUDIO_OUTPUT_HISTORY_SECS;
    audio->replay_buf = calloc(audio->replay_max_len, sizeof(float));
    audio->replay_buf_back = calloc(audio->replay_max_len, sizeof(float));
    audio->replay_len = 0;
    audio->replay_rate = AUDIO_OUTPUT_HISTORY_RATE;
    memset(&audio->replay_voice, 0, sizeof(audio->replay_voice));
    audio->mic_connected = 0;
    audio->mic_hot = 0;
    audio->mic_level = 0.0f;
    audio->mic_last_chunk = 0;
    audio->capture_thread_running = 0;
    memset(audio->mic_waveform, 0, sizeof(audio->mic_waveform));
    audio->mic_waveform_pos = 0;
    audio->mic_ring = calloc(audio->sample_max_len, sizeof(float));
    audio->mic_ring_pos = 0;
    audio->rec_start_ring_pos = 0;
    audio->output_history_buf = calloc(AUDIO_OUTPUT_HISTORY_RATE * AUDIO_OUTPUT_HISTORY_SECS, sizeof(float));
    audio->output_history_size = AUDIO_OUTPUT_HISTORY_RATE * AUDIO_OUTPUT_HISTORY_SECS;
    audio->output_history_rate = AUDIO_OUTPUT_HISTORY_RATE;
    audio->output_history_downsample_n = 1;
    audio->output_history_downsample_pos = 0;
    audio->output_history_write_pos = 0;
    snprintf(audio->mic_device, sizeof(audio->mic_device), "none");
    audio->mic_last_error[0] = 0;
    seed_default_sample(audio);

    // DJ decks: initialize with default volumes
    audio->crossfader = 0.5f;         // centered
    audio->deck_master_volume = 0.8f; // default master
    for (int d = 0; d < AUDIO_MAX_DECKS; d++) {
        audio->decks[d].active = 0;
        audio->decks[d].playing = 0;
        audio->decks[d].volume = 1.0f;
        audio->decks[d].decoder = NULL;
    }

    // TTS PCM ring buffer (5 seconds at max output rate)
    audio->tts_buf_size = AUDIO_SAMPLE_RATE * 5;  // allocated at max, actual_rate adjusts usage
    audio->tts_buf = calloc(audio->tts_buf_size, sizeof(float));
    audio->tts_read_pos = 0;
    audio->tts_write_pos = 0;
    audio->tts_volume = 2.5f;  // Boost flite output (naturally quiet)

    snprintf(audio->audio_device, sizeof(audio->audio_device), "none");
    snprintf(audio->audio_status, sizeof(audio->audio_status), "initializing");
    audio->audio_init_retries = 0;

    // Wait for sound card to appear
    fprintf(stderr, "[audio] Waiting for sound card...\n");
    int card_found = 0;
    for (int w = 0; w < 400; w++) { // up to 8 seconds
        if (access("/dev/snd/pcmC0D0p", F_OK) == 0 ||
            access("/dev/snd/pcmC1D0p", F_OK) == 0 ||
            access("/dev/snd/pcmC2D0p", F_OK) == 0) { card_found = 1; break; }
        usleep(20000);
    }
    if (!card_found) {
        // Distinguish: HDA controller present (codec probe failed) vs no hardware at all
        if (access("/dev/snd/controlC0", F_OK) == 0) {
            fprintf(stderr, "[audio] WARNING: HDA controller present but codec not probed after 8s\n");
            snprintf(audio->audio_status, sizeof(audio->audio_status), "HDA ctrl ok, codec not probed");
        } else {
            fprintf(stderr, "[audio] WARNING: no sound card after 8s wait\n");
            snprintf(audio->audio_status, sizeof(audio->audio_status), "no card (8s timeout)");
        }
    }

    // Dump sound card info for diagnostics (write to USB log if mounted)
    FILE *alog = fopen("/mnt/ac-audio.log", "w");
    if (!alog) alog = stderr;  // fallback to stderr
    {
        FILE *cards = fopen("/proc/asound/cards", "r");
        if (cards) {
            char line[256];
            fprintf(alog, "[audio] === /proc/asound/cards ===\n");
            while (fgets(line, sizeof(line), cards))
                fprintf(alog, "[audio] %s", line);
            fclose(cards);
        } else {
            fprintf(alog, "[audio] WARNING: /proc/asound/cards not found!\n");
        }
        // Also check /dev/snd/
        DIR *snddir = opendir("/dev/snd");
        if (snddir) {
            struct dirent *ent;
            fprintf(alog, "[audio] /dev/snd/:");
            while ((ent = readdir(snddir))) {
                if (ent->d_name[0] != '.') fprintf(alog, " %s", ent->d_name);
            }
            fprintf(alog, "\n");
            closedir(snddir);
        } else {
            fprintf(alog, "[audio] WARNING: /dev/snd/ not found!\n");
        }
    }

    // Open ALSA — try multiple cards and devices, with retries for race conditions.
    // On fast NVMe boots the HDA codec may not be fully probed when we first try.
    // If AC_AUDIO_DEVICE is set, try it first (used for stream tee via asound.conf).
    snd_pcm_t *pcm = NULL;
    const char *devices[] = {
        "hw:0,0", "hw:1,0", "hw:0,1", "hw:1,1",
        "hw:0,2", "hw:0,3", "hw:1,2", "hw:1,3",
        "plughw:0,0", "plughw:1,0",
        "default", NULL
    };
    int err = -1;
    int card_idx = 0;

    // AC_AUDIO_DEVICE override — try the env var device before the hardcoded list.
    const char *env_dev = getenv("AC_AUDIO_DEVICE");
    if (env_dev && env_dev[0]) {
        err = snd_pcm_open(&pcm, env_dev, SND_PCM_STREAM_PLAYBACK, 0);
        if (err >= 0) {
            fprintf(stderr, "[audio] Opened AC_AUDIO_DEVICE=%s\n", env_dev);
            snprintf(audio->audio_device, sizeof(audio->audio_device), "%s", env_dev);
            if (sscanf(env_dev, "hw:%d", &card_idx) != 1 &&
                sscanf(env_dev, "plughw:%d", &card_idx) != 1)
                card_idx = 0;
        } else {
            fprintf(stderr, "[audio] AC_AUDIO_DEVICE=%s failed: %s — falling back\n",
                    env_dev, snd_strerror(err));
        }
    }

    for (int attempt = 0; attempt < 5 && err < 0; attempt++) {
        if (attempt > 0) {
            fprintf(alog, "[audio] Retry %d/4 — waiting 2s for codec probe...\n", attempt);
            fprintf(stderr, "[audio] Retry %d/4 — waiting 2s for codec probe...\n", attempt);
            usleep(2000000); // 2 seconds between retries
        }
        for (int i = 0; devices[i]; i++) {
            audio->audio_init_retries++;
            err = snd_pcm_open(&pcm, devices[i], SND_PCM_STREAM_PLAYBACK, 0);
            if (err >= 0) {
                fprintf(alog, "[audio] Opened ALSA device: %s (attempt %d)\n", devices[i], attempt);
                fprintf(stderr, "[audio] Opened ALSA device: %s (attempt %d)\n", devices[i], attempt);
                snprintf(audio->audio_device, sizeof(audio->audio_device), "%s", devices[i]);
                if (sscanf(devices[i], "hw:%d", &card_idx) != 1 &&
                    sscanf(devices[i], "plughw:%d", &card_idx) != 1)
                    card_idx = 0;
                break;
            }
            if (attempt == 0)
                fprintf(alog, "[audio] Failed %s: %s\n", devices[i], snd_strerror(err));
        }
    }
    audio->card_index = card_idx;
    if (alog != stderr) { fflush(alog); fclose(alog); }
    if (err < 0) {
        fprintf(stderr, "[audio] Cannot open any ALSA device after 5 attempts\n");
        snprintf(audio->audio_status, sizeof(audio->audio_status), "no ALSA device found");
        // Audio is optional — return the struct but with no PCM
        audio->pcm = NULL;
        return audio;
    }

    // Configure ALSA — negotiate rate dynamically.
    // Try preferred rates from highest to lowest. The hardware decides what it
    // actually supports; we adapt period/buffer sizes to match the negotiated rate.
    snd_pcm_hw_params_t *params;
    snd_pcm_hw_params_alloca(&params);
    snd_pcm_hw_params_any(pcm, params);
    snd_pcm_hw_params_set_access(pcm, params, SND_PCM_ACCESS_RW_INTERLEAVED);
    snd_pcm_hw_params_set_format(pcm, params, SND_PCM_FORMAT_S16_LE);
    snd_pcm_hw_params_set_channels(pcm, params, AUDIO_CHANNELS);

    // Query hardware rate range
    unsigned int rate_min = 0, rate_max = 0;
    snd_pcm_hw_params_get_rate_min(params, &rate_min, NULL);
    snd_pcm_hw_params_get_rate_max(params, &rate_max, NULL);
    fprintf(stderr, "[audio] Hardware rate range: %u–%u Hz\n", rate_min, rate_max);

    // Pick sample rate: 48kHz is the safe default that all hardware can sustain.
    // Many codecs (e.g. Cirrus Logic CS4206) claim 192kHz support but can't
    // sustain it without constant XRUNs. Only use high rates on known-good
    // hardware (ThinkPad HDA with Realtek codec handles 192kHz fine).
    // Heuristic: if max rate > 48kHz AND min rate <= 32kHz, the codec is
    // likely a laptop HDA that works better at 48kHz.
    unsigned int rate = 48000;
    if (rate_max >= 192000 && rate_min > 44100) {
        // Dedicated audio interface — likely supports high rates reliably
        rate = 192000;
    } else if (rate_max >= 96000 && rate_min > 44100) {
        rate = 96000;
    }
    // Override: environment variable AC_AUDIO_RATE forces a specific rate
    const char *env_rate = getenv("AC_AUDIO_RATE");
    if (env_rate) {
        unsigned int r = (unsigned int)atoi(env_rate);
        if (r >= rate_min && r <= rate_max) rate = r;
    }
    fprintf(stderr, "[audio] Selected rate: %u Hz (hw range %u–%u)\n", rate, rate_min, rate_max);
    snd_pcm_hw_params_set_rate_near(pcm, params, &rate, 0);

    // Scale period and buffer to ~1ms latency at the negotiated rate
    snd_pcm_uframes_t period = rate / 1000;  // ~1ms worth of frames
    if (period < 64) period = 64;
    snd_pcm_hw_params_set_period_size_near(pcm, params, &period, 0);

    snd_pcm_uframes_t buffer_size = period * 4;  // 4 periods (~4ms total)
    snd_pcm_hw_params_set_buffer_size_near(pcm, params, &buffer_size);

    err = snd_pcm_hw_params(pcm, params);
    if (err < 0) {
        fprintf(stderr, "[audio] Cannot configure ALSA at %uHz: %s\n", rate, snd_strerror(err));
        // Last resort: try plughw with default params
        fprintf(stderr, "[audio] Trying plughw fallback...\n");
        snd_pcm_close(pcm);
        err = snd_pcm_open(&pcm, "plughw:0,0", SND_PCM_STREAM_PLAYBACK, 0);
        if (err >= 0) {
            snd_pcm_hw_params_any(pcm, params);
            snd_pcm_hw_params_set_access(pcm, params, SND_PCM_ACCESS_RW_INTERLEAVED);
            snd_pcm_hw_params_set_format(pcm, params, SND_PCM_FORMAT_S16_LE);
            snd_pcm_hw_params_set_channels(pcm, params, AUDIO_CHANNELS);
            rate = 48000;
            snd_pcm_hw_params_set_rate_near(pcm, params, &rate, 0);
            period = 256;
            snd_pcm_hw_params_set_period_size_near(pcm, params, &period, 0);
            buffer_size = 1024;
            snd_pcm_hw_params_set_buffer_size_near(pcm, params, &buffer_size);
            err = snd_pcm_hw_params(pcm, params);
        }
        if (err < 0) {
            fprintf(stderr, "[audio] All ALSA config attempts failed: %s\n", snd_strerror(err));
            snd_pcm_close(pcm);
            audio->pcm = NULL;
            return audio;
        }
        snprintf(audio->audio_device, sizeof(audio->audio_device), "plughw:0,0");
    }

    snd_pcm_prepare(pcm);
    audio->pcm = pcm;
    audio->actual_rate = rate;
    audio->actual_period = (unsigned int)period;

    // Update glitch rate for actual sample rate
    audio->glitch_rate = rate / 1600;

    // Recent output history targets ~48k mono regardless of playback rate.
    unsigned int hist_target = rate > AUDIO_OUTPUT_HISTORY_RATE ? AUDIO_OUTPUT_HISTORY_RATE : rate;
    unsigned int hist_stride = rate > hist_target ? (rate + hist_target / 2) / hist_target : 1;
    if (hist_stride == 0) hist_stride = 1;
    audio->output_history_rate = rate / hist_stride;
    if (audio->output_history_rate == 0) audio->output_history_rate = rate;
    audio->output_history_downsample_n = hist_stride;
    audio->output_history_downsample_pos = 0;
    audio->output_history_size = (int)(audio->output_history_rate * AUDIO_OUTPUT_HISTORY_SECS);
    if (audio->output_history_size <= 0) {
        audio->output_history_size = AUDIO_OUTPUT_HISTORY_RATE * AUDIO_OUTPUT_HISTORY_SECS;
        audio->output_history_rate = AUDIO_OUTPUT_HISTORY_RATE;
        audio->output_history_downsample_n = 1;
    }

    // Reallocate room buffers for actual rate
    int actual_room_size = (int)(0.12 * rate) * 3;
    if (actual_room_size != audio->room_size) {
        free(audio->room_buf_l); free(audio->room_buf_r);
        audio->room_size = actual_room_size;
        audio->room_buf_l = calloc(actual_room_size, sizeof(float));
        audio->room_buf_r = calloc(actual_room_size, sizeof(float));
        audio->room_pos = 0;
    }

    fprintf(stderr, "[audio] ALSA: requested %dHz, got %uHz, period=%lu, buffer=%lu (%.1fms latency)\n",
            AUDIO_SAMPLE_RATE, rate, (unsigned long)period, (unsigned long)buffer_size,
            (double)period / rate * 1000.0);
    snprintf(audio->audio_status, sizeof(audio->audio_status),
             "ok %uHz %lufrm", rate, (unsigned long)period);
    if (rate != AUDIO_SAMPLE_RATE)
        fprintf(stderr, "[audio] WARNING: got %uHz instead of %dHz\n", rate, AUDIO_SAMPLE_RATE);

    // Unmute ALL outputs (HDA Intel codecs have many controls that can mute)
    char mixer_card[16];
    snprintf(mixer_card, sizeof(mixer_card), "hw:%d", card_idx);
    fprintf(stderr, "[audio] Using mixer: %s\n", mixer_card);

    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) >= 0) {
        snd_mixer_attach(mixer, mixer_card);
        snd_mixer_selem_register(mixer, NULL, NULL);
        snd_mixer_load(mixer);

        snd_mixer_elem_t *elem;
        for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
            const char *name = snd_mixer_selem_get_name(elem);
            if (!snd_mixer_selem_is_active(elem)) continue;

            // Log all mixer elements
            fprintf(stderr, "[audio] Mixer: %s", name);
            if (snd_mixer_selem_has_playback_volume(elem)) fprintf(stderr, " [vol]");
            if (snd_mixer_selem_has_playback_switch(elem)) fprintf(stderr, " [sw]");
            if (snd_mixer_selem_has_capture_switch(elem)) fprintf(stderr, " [cap-sw]");
            if (snd_mixer_selem_has_capture_volume(elem)) fprintf(stderr, " [cap-vol]");
            fprintf(stderr, "\n");

            // Unmute every playback switch we find
            if (snd_mixer_selem_has_playback_switch(elem)) {
                snd_mixer_selem_set_playback_switch_all(elem, 1);
                fprintf(stderr, "[audio] Unmuted: %s\n", name);
            }

            // Set volume to max for output controls
            if (snd_mixer_selem_has_playback_volume(elem)) {
                long min, max;
                snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
                snd_mixer_selem_set_playback_volume_all(elem, max);
                fprintf(stderr, "[audio] Volume %s: %ld/%ld\n", name, max, max);
            }

            // NOTE: Do NOT touch capture mixer controls here — on the 11e Yoga
            // Gen 5, enabling capture switches at boot (before any capture PCM
            // is open) puts the HDA codec into a bad state that causes EIO when
            // the capture stream is later opened with period/buffer params.
        }
        snd_mixer_close(mixer);
    } else {
        fprintf(stderr, "[audio] Cannot open mixer\n");
    }

    // Read initial system volume
    audio->system_volume = read_system_volume_card(card_idx);
    fprintf(stderr, "[audio] System volume: %d%%\n", audio->system_volume);

    // HDMI audio disabled — opening HDMI PCM streams on the same HDA controller
    // can exhaust controller streams and cause EIO on capture.
    audio->hdmi_pcm = NULL;
    fprintf(stderr, "[audio] HDMI audio: disabled\n");

    // Start audio thread
    audio->running = 1;
    pthread_create(&audio->thread, NULL, audio_thread_fn, audio);

    fprintf(stderr, "[audio] Ready\n");
    return audio;
}

uint64_t audio_synth(ACAudio *audio, WaveType type, double freq,
                     double duration, double volume, double attack,
                     double decay, double pan) {
    if (!audio) return 0;

    pthread_mutex_lock(&audio->lock);

    // Find free voice slot
    int slot = -1;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].state == VOICE_INACTIVE) {
            slot = i;
            break;
        }
    }
    if (slot < 0) {
        // Steal oldest voice
        double oldest = 0;
        slot = 0;
        for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
            if (audio->voices[i].elapsed > oldest) {
                oldest = audio->voices[i].elapsed;
                slot = i;
            }
        }
    }

    ACVoice *v = &audio->voices[slot];
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
    v->id = ++audio->next_id;
    v->started_at = audio->time;

    if (type == WAVE_NOISE || type == WAVE_WHISTLE || type == WAVE_GUN) {
        v->noise_seed = (uint32_t)(audio->next_id * 2654435761u);
    }
    if (type == WAVE_NOISE) {
        setup_noise_filter(v, (double)(audio->actual_rate ? audio->actual_rate : AUDIO_SAMPLE_RATE));
    } else if (type == WAVE_GUN) {
        // Caller (audio_synth_gun) sets the preset via gun_init_voice
        // after this base init runs.
    } else if (type == WAVE_WHISTLE) {
        // Clear the waveguide state — bore + jet delay buffers and the
        // loop filter / DC blocker. Without this, leftover state from a
        // previous voice reuse would produce startup artifacts.
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

    pthread_mutex_unlock(&audio->lock);
    return v->id;
}

void audio_kill(ACAudio *audio, uint64_t id, double fade) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id && audio->voices[i].state == VOICE_ACTIVE) {
            audio->voices[i].state = VOICE_KILLING;
            audio->voices[i].fade_duration = fade > 0 ? fade : 0.025;
            audio->voices[i].fade_elapsed = 0.0;
            // Gun-specific release behaviors (e.g. ricochet pitch drop).
            if (audio->voices[i].type == WAVE_GUN) {
                gun_on_release(&audio->voices[i]);
            }
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

uint64_t audio_synth_gun(ACAudio *audio, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model) {
    if (!audio) return 0;
    // Delegate base voice setup (slot alloc, envelope fields, noise seed).
    // Frequency is unused for guns — the DWG cavity resonance comes from
    // the preset's bore_length, not v->frequency. We pass 110 to keep
    // the smoothing code happy.
    uint64_t id = audio_synth(audio, WAVE_GUN, 110.0, duration, volume,
                              attack, decay, pan);
    if (!id) return 0;

    pthread_mutex_lock(&audio->lock);
    ACVoice *v = NULL;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id) { v = &audio->voices[i]; break; }
    }
    if (v) {
        double sr = (double)(audio->actual_rate ? audio->actual_rate
                                                : AUDIO_SAMPLE_RATE);
        gun_init_voice(v, preset, sr, force_model);
        if (pressure_scale > 0.0 && pressure_scale != 1.0) {
            v->gun_pressure *= pressure_scale;
        }
    }
    pthread_mutex_unlock(&audio->lock);
    return id;
}

void audio_update(ACAudio *audio, uint64_t id, double freq,
                  double volume, double pan) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id && audio->voices[i].state != VOICE_INACTIVE) {
            if (freq > 0) audio->voices[i].target_frequency = freq;
            if (volume >= 0) audio->voices[i].volume = volume;
            if (pan > -2.0) audio->voices[i].pan = pan;
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

int audio_beat_check(ACAudio *audio) {
    if (!audio) return 0;
    int triggered = audio->beat_triggered;
    if (triggered) audio->beat_triggered = 0;
    return triggered;
}

void audio_set_bpm(ACAudio *audio, double bpm) {
    if (!audio || bpm <= 0) return;
    pthread_mutex_lock(&audio->lock);
    audio->bpm = bpm;
    pthread_mutex_unlock(&audio->lock);
}

void audio_room_toggle(ACAudio *audio) {
    if (!audio) return;
    audio->room_enabled = !audio->room_enabled;
    fprintf(stderr, "[audio] Room: %s\n", audio->room_enabled ? "ON" : "OFF");
}

void audio_glitch_toggle(ACAudio *audio) {
    if (!audio) return;
    if (audio->glitch_enabled || audio->target_glitch_mix > 0.001f || audio->glitch_mix > 0.001f) {
        audio->glitch_enabled = 0;
        audio->target_glitch_mix = 0.0f;
    } else {
        audio->glitch_enabled = 1;
        audio->target_glitch_mix = 1.0f;
    }
    fprintf(stderr, "[audio] Glitch: %s (mix %.2f)\n",
            audio->glitch_enabled ? "ON" : "OFF",
            audio->target_glitch_mix);
}

void audio_set_room_mix(ACAudio *audio, float mix) {
    if (!audio) return;
    if (mix < 0.0f) mix = 0.0f;
    if (mix > 1.0f) mix = 1.0f;
    audio->target_room_mix = mix;
}

void audio_set_glitch_mix(ACAudio *audio, float mix) {
    if (!audio) return;
    if (mix < 0.0f) mix = 0.0f;
    if (mix > 1.0f) mix = 1.0f;
    audio->target_glitch_mix = mix;
    audio->glitch_enabled = mix > 0.001f;
    if (!audio->glitch_enabled) audio->glitch_counter = 0;
}

void audio_set_fx_mix(ACAudio *audio, float mix) {
    if (!audio) return;
    if (mix < 0.0f) mix = 0.0f;
    if (mix > 1.0f) mix = 1.0f;
    audio->target_fx_mix = mix;
}

// --- Hot-mic capture thread ---
// Device opens once (on wave-enter), stays running. Always reads to keep
// ALSA happy and the level meter live. Only writes to sample_buf when
// recording flag is set. Instant recording with zero device-open latency.
//
// IMPORTANT: HDMI audio must be DISABLED in audio_init and playback buffer
// must be 3 periods (not 6) — otherwise the HDA controller runs out of
// streams and capture gets EIO.
static void *capture_thread_func(void *arg) {
    ACAudio *audio = (ACAudio *)arg;
    snd_pcm_t *cap = NULL;

    const char *devices[] = {"hw:0,0", "hw:1,0", "hw:0,6", "hw:0,7",
                             "plughw:0,0", "plughw:1,0", "default", NULL};
    for (int i = 0; devices[i]; i++) {
        if (snd_pcm_open(&cap, devices[i], SND_PCM_STREAM_CAPTURE, 0) == 0) {
            snprintf(audio->mic_device, sizeof(audio->mic_device), "%s", devices[i]);
            ac_log("[mic] opened capture device: %s\n", devices[i]);
            break;
        }
        cap = NULL;
    }
    if (!cap) {
        snprintf(audio->mic_last_error, sizeof(audio->mic_last_error),
                 "no capture device found");
        ac_log("[mic] no capture device found\n");
        audio->mic_hot = 0;
        return NULL;
    }

    // Enable capture mixer switches now that capture PCM is open.
    // (Safe to do after snd_pcm_open — avoids the pre-open EIO bug on some HDA.)
    {
        char mixer_card[16];
        snprintf(mixer_card, sizeof(mixer_card), "hw:%d", audio->card_index);
        snd_mixer_t *cmix = NULL;
        if (snd_mixer_open(&cmix, 0) >= 0) {
            snd_mixer_attach(cmix, mixer_card);
            snd_mixer_selem_register(cmix, NULL, NULL);
            snd_mixer_load(cmix);
            snd_mixer_elem_t *el;
            for (el = snd_mixer_first_elem(cmix); el; el = snd_mixer_elem_next(el)) {
                if (!snd_mixer_selem_is_active(el)) continue;
                if (snd_mixer_selem_has_capture_switch(el)) {
                    snd_mixer_selem_set_capture_switch_all(el, 1);
                    ac_log("[mic] enabled capture switch: %s\n", snd_mixer_selem_get_name(el));
                }
                if (snd_mixer_selem_has_capture_volume(el)) {
                    long cmin, cmax;
                    snd_mixer_selem_get_capture_volume_range(el, &cmin, &cmax);
                    snd_mixer_selem_set_capture_volume_all(el, cmax);
                    ac_log("[mic] capture volume %s: %ld/%ld\n", snd_mixer_selem_get_name(el), cmax, cmax);
                }
            }
            snd_mixer_close(cmix);
        }
    }

    snd_pcm_hw_params_t *hw;
    snd_pcm_hw_params_alloca(&hw);
    snd_pcm_hw_params_any(cap, hw);
    snd_pcm_hw_params_set_access(cap, hw, SND_PCM_ACCESS_RW_INTERLEAVED);
    snd_pcm_hw_params_set_format(cap, hw, SND_PCM_FORMAT_S16_LE);

    unsigned int channels = 1;
    if (snd_pcm_hw_params_set_channels(cap, hw, 1) < 0) {
        channels = 2;
        snd_pcm_hw_params_set_channels(cap, hw, 2);
    }

    unsigned int rate = 48000;
    snd_pcm_hw_params_set_rate_near(cap, hw, &rate, NULL);

    // Set period=1024 (~21ms) for low-latency capture. This previously
    // caused EIO but the root causes were: HDMI audio open (exhausting
    // HDA streams), 6-period playback buffer, and capture mixer in
    // audio_init. All three are now fixed.
    snd_pcm_uframes_t period_frames = 1024;
    snd_pcm_hw_params_set_period_size_near(cap, hw, &period_frames, NULL);
    snd_pcm_uframes_t buffer_frames = 8192;
    snd_pcm_hw_params_set_buffer_size_near(cap, hw, &buffer_frames);

    if (snd_pcm_hw_params(cap, hw) < 0) {
        snprintf(audio->mic_last_error, sizeof(audio->mic_last_error),
                 "failed to configure capture");
        ac_log("[mic] failed to configure capture\n");
        snd_pcm_close(cap);
        audio->mic_hot = 0;
        return NULL;
    }

    // Enable capture mixer (safe here — after PCM open, in capture thread)
    {
        int cnum = 0;
        const char *d = audio->mic_device;
        while (*d && (*d < '0' || *d > '9')) d++;
        if (*d) cnum = atoi(d);
        char ccard[16];
        snprintf(ccard, sizeof(ccard), "hw:%d", cnum);
        snd_mixer_t *cmix = NULL;
        if (snd_mixer_open(&cmix, 0) >= 0) {
            snd_mixer_attach(cmix, ccard);
            snd_mixer_selem_register(cmix, NULL, NULL);
            snd_mixer_load(cmix);
            snd_mixer_elem_t *elem;
            for (elem = snd_mixer_first_elem(cmix); elem; elem = snd_mixer_elem_next(elem)) {
                if (!snd_mixer_selem_is_active(elem)) continue;
                if (snd_mixer_selem_has_capture_switch(elem)) {
                    snd_mixer_selem_set_capture_switch_all(elem, 1);
                    ac_log("[mic] capture switch ON: %s\n", snd_mixer_selem_get_name(elem));
                }
                if (snd_mixer_selem_has_capture_volume(elem)) {
                    long cmin, cmax;
                    snd_mixer_selem_get_capture_volume_range(elem, &cmin, &cmax);
                    long cset = cmin + ((cmax - cmin) * 9) / 10;
                    snd_mixer_selem_set_capture_volume_all(elem, cset);
                    ac_log("[mic] capture volume %s: %ld/%ld\n",
                           snd_mixer_selem_get_name(elem), cset, cmax);
                }
            }
            snd_mixer_close(cmix);
        }
    }

    audio->sample_rate = rate;
    audio->mic_connected = 1;
    ac_log("[mic] hot-mic running at %u Hz, %u ch\n", rate, channels);

    int16_t buf[1024 * 2];
    while (audio->mic_hot) {
        int n = snd_pcm_readi(cap, buf, 512);
        if (n < 0) {
            n = snd_pcm_recover(cap, n, 0);
            if (n < 0) {
                snprintf(audio->mic_last_error, sizeof(audio->mic_last_error),
                         "capture read failed: %s", snd_strerror(n));
                ac_log("[mic] capture read failed: %s\n", snd_strerror(n));
                break;
            }
            continue;
        }

        float peak = 0.0f;
        // Aggressive compressor + hard limiter to prevent clipping.
        // Matches the note compression style in the synth output.
        static float env = 0.0f;         // envelope follower
        static float comp_gain = 1.0f;   // current gain
        const float threshold = 0.15f;   // compress early (mic input is often hot)
        const float ratio = 12.0f;       // aggressive compression
        const float attack = 0.005f;     // fast attack
        const float release = 0.00005f;  // slow release (smooth)
        const float limiter = 0.9f;      // hard limiter ceiling

        for (int s = 0; s < n; s++) {
            float sample;
            if (channels == 1) {
                sample = buf[s] / 32768.0f;
            } else {
                sample = (buf[s * 2] + buf[s * 2 + 1]) / 65536.0f;
            }

            // Envelope follower
            float abs_s = fabsf(sample);
            if (abs_s > env)
                env += attack * (abs_s - env);
            else
                env += release * (abs_s - env);

            // Compute gain reduction
            if (env > threshold) {
                float over = env - threshold;
                float reduced = threshold + over / ratio;
                comp_gain = reduced / env;
            } else {
                comp_gain += 0.0002f * (1.0f - comp_gain);
            }

            sample *= comp_gain;

            // Hard limiter — prevent any clipping
            if (sample > limiter) sample = limiter;
            else if (sample < -limiter) sample = -limiter;

            if (abs_s > peak) peak = abs_s;

            // Always write to ring buffer
            audio->mic_ring[audio->mic_ring_pos % audio->sample_max_len] = sample;
            audio->mic_ring_pos++;

            // Direct-write when recording
            if (audio->recording && audio->sample_write_pos < audio->sample_max_len) {
                audio->sample_buf[audio->sample_write_pos++] = sample;
            }
        }
        // If we skipped the first chunk, mark that we've consumed it
        // by writing at least 0 (sample_write_pos stays 0, next chunk writes)
        audio->mic_level = peak;

        if (audio->recording && audio->sample_write_pos >= audio->sample_max_len) {
            audio->sample_len = audio->sample_write_pos;
            audio->recording = 0;
            ac_log("[mic] recording buffer full (%d samples)\n", audio->sample_len);
        }
    }

    ac_log("[mic] hot-mic thread exiting, device=%s\n", audio->mic_device);
    snd_pcm_close(cap);
    audio->mic_connected = 0;
    audio->recording = 0;
    return NULL;
}

int audio_mic_open(ACAudio *audio) {
    if (!audio || audio->mic_hot || audio->capture_thread_running) return -1;
    audio->mic_hot = 1;
    audio->capture_thread_running = 1;
    audio->mic_last_error[0] = 0;
    ac_log("[mic] opening hot-mic\n");
    if (pthread_create(&audio->capture_thread, NULL, capture_thread_func, audio) != 0) {
        audio->mic_hot = 0;
        audio->capture_thread_running = 0;
        ac_log("[mic] failed to create capture thread\n");
        return -1;
    }
    return 0;
}

void audio_mic_close(ACAudio *audio) {
    if (!audio) return;
    audio->recording = 0;
    audio->mic_hot = 0;
    if (audio->capture_thread_running) {
        pthread_join(audio->capture_thread, NULL);
        audio->capture_thread_running = 0;
    }
    ac_log("[mic] hot-mic closed\n");
}

int audio_mic_start(ACAudio *audio) {
    if (!audio || audio->recording) return -1;
    if (!audio->mic_hot) {
        int rc = audio_mic_open(audio);
        if (rc != 0) return rc;
    }
    // Kill any playing sample voices
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++)
        audio->sample_voices[i].active = 0;
    audio->rec_start_ring_pos = audio->mic_ring_pos;
    audio->sample_len = 0;
    audio->sample_write_pos = 0;
    __sync_synchronize();
    audio->recording = 1;
    ac_log("[mic] recording started (instant), ring_pos=%d\n", audio->rec_start_ring_pos);
    return 0;
}

int audio_mic_stop(ACAudio *audio) {
    if (!audio) return 0;
    audio->recording = 0;
    __sync_synchronize();

    // Kill all sample voices BEFORE touching sample_buf —
    // playback thread reads sample_buf[]/sample_len without locks
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++)
        audio->sample_voices[i].active = 0;
    __sync_synchronize();

    int direct_len = audio->sample_write_pos;
    if (direct_len > 0) {
        audio->sample_len = direct_len;
        ac_log("[mic] recording stopped (direct), sample_len=%d sample_rate=%u\n",
               audio->sample_len, audio->sample_rate);
    } else {
        // Fallback: extract from ring buffer
        int start = audio->rec_start_ring_pos;
        int end = audio->mic_ring_pos;
        int len = end - start;
        if (len < 0) len = 0;
        if (len > audio->sample_max_len) len = audio->sample_max_len;
        for (int i = 0; i < len; i++) {
            audio->sample_buf[i] = audio->mic_ring[(start + i) % audio->sample_max_len];
        }
        audio->sample_len = len;
        ac_log("[mic] recording stopped (ring), sample_len=%d ring_span=%d sample_rate=%u\n",
               audio->sample_len, end - start, audio->sample_rate);
    }
    // Auto-trim silence from start (threshold: ~0.01 = -40dB)
    if (audio->sample_len > 0) {
        const float trim_threshold = 0.01f;
        int trim_start = 0;
        while (trim_start < audio->sample_len &&
               fabsf(audio->sample_buf[trim_start]) < trim_threshold) {
            trim_start++;
        }
        if (trim_start > 0 && trim_start < audio->sample_len) {
            int new_len = audio->sample_len - trim_start;
            memmove(audio->sample_buf, audio->sample_buf + trim_start,
                    new_len * sizeof(float));
            audio->sample_len = new_len;
            ac_log("[mic] auto-trimmed %d silent samples from start\n", trim_start);
        }
    }

    return audio->sample_len;
}

// --- Sample bank: get/load data for per-key samples ---
int audio_sample_get_data(ACAudio *audio, float *out, int max_len) {
    if (!audio || !out || audio->sample_len == 0) return 0;
    int len = audio->sample_len < max_len ? audio->sample_len : max_len;
    memcpy(out, audio->sample_buf, len * sizeof(float));
    return len;
}

int audio_output_get_recent(ACAudio *audio, float *out, int max_len, unsigned int *out_rate) {
    if (!audio || !out || max_len <= 0 || !audio->output_history_buf || audio->output_history_size <= 0) {
        if (out_rate) *out_rate = 0;
        return 0;
    }

    pthread_mutex_lock(&audio->lock);
    if (out_rate) *out_rate = audio->output_history_rate;

    uint64_t write_pos = audio->output_history_write_pos;
    int available = write_pos < (uint64_t)audio->output_history_size
        ? (int)write_pos
        : audio->output_history_size;
    int len = available < max_len ? available : max_len;
    uint64_t start = write_pos - (uint64_t)len;
    for (int i = 0; i < len; i++) {
        out[i] = audio->output_history_buf[(start + (uint64_t)i) % (uint64_t)audio->output_history_size];
    }

    pthread_mutex_unlock(&audio->lock);
    return len;
}

void audio_sample_load_data(ACAudio *audio, const float *data, int len, unsigned int rate) {
    if (!audio || !data || len <= 0 || !audio->sample_buf_back) return;
    if (len > audio->sample_max_len) len = audio->sample_max_len;
    // Write to back buffer (only JS thread writes here — safe without lock)
    memcpy(audio->sample_buf_back, data, len * sizeof(float));
    if (len < audio->sample_max_len)
        memset(audio->sample_buf_back + len, 0, (audio->sample_max_len - len) * sizeof(float));
    // Swap pointers under lock — audio callback checks sample_loading flag
    pthread_mutex_lock(&audio->lock);
    float *tmp = audio->sample_buf;
    audio->sample_buf = audio->sample_buf_back;
    audio->sample_buf_back = tmp;
    audio->sample_len = len;
    if (rate > 0) audio->sample_rate = rate;
    __sync_synchronize();
    pthread_mutex_unlock(&audio->lock);
    // Log peak value and first few samples for debugging
    float peak = 0.0f;
    for (int i = 0; i < len; i++) {
        float a = fabsf(audio->sample_buf[i]);
        if (a > peak) peak = a;
    }
    ac_log("[sample] loaded %d samples (%d Hz) peak=%.4f first=[%.3f,%.3f,%.3f,%.3f]\n",
           len, audio->sample_rate, peak,
           len > 0 ? audio->sample_buf[0] : 0,
           len > 1 ? audio->sample_buf[1] : 0,
           len > 2 ? audio->sample_buf[2] : 0,
           len > 3 ? audio->sample_buf[3] : 0);
}

void audio_replay_load_data(ACAudio *audio, const float *data, int len, unsigned int rate) {
    if (!audio || !data || len <= 0 || !audio->replay_buf_back) return;
    if (len > audio->replay_max_len) len = audio->replay_max_len;

    memcpy(audio->replay_buf_back, data, len * sizeof(float));
    if (len < audio->replay_max_len)
        memset(audio->replay_buf_back + len, 0, (audio->replay_max_len - len) * sizeof(float));

    pthread_mutex_lock(&audio->lock);
    audio->replay_voice.active = 0;
    float *tmp = audio->replay_buf;
    audio->replay_buf = audio->replay_buf_back;
    audio->replay_buf_back = tmp;
    audio->replay_len = len;
    if (rate > 0) audio->replay_rate = rate;
    __sync_synchronize();
    pthread_mutex_unlock(&audio->lock);
}

// --- Sample playback ---
uint64_t audio_sample_play(ACAudio *audio, double freq, double base_freq,
                           double volume, double pan, int loop) {
    if (!audio || audio->sample_len == 0) return 0;
    pthread_mutex_lock(&audio->lock);

    // Find free slot (or steal oldest)
    int slot = -1;
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++) {
        if (!audio->sample_voices[i].active) { slot = i; break; }
    }
    if (slot < 0) slot = 0; // steal first

    SampleVoice *sv = &audio->sample_voices[slot];
    sv->active = 1;
    sv->loop = loop;
    sv->position = 0.0;
    // Speed: pitch ratio * rate conversion (capture rate → output rate)
    sv->speed = (freq / base_freq) * ((double)audio->sample_rate / (double)audio->actual_rate);
    sv->volume = volume;
    sv->pan = pan;
    sv->fade = 0.0;
    sv->fade_target = 1.0;
    sv->id = audio->sample_next_id++;

    pthread_mutex_unlock(&audio->lock);
    ac_log("[sample] play freq=%.1f base=%.1f speed=%.4f rate=%u/%u len=%d id=%lu\n",
           freq, base_freq, sv->speed, audio->sample_rate, audio->actual_rate,
           audio->sample_len, (unsigned long)sv->id);
    return sv->id;
}

uint64_t audio_replay_play(ACAudio *audio, double freq, double base_freq,
                           double volume, double pan, int loop) {
    if (!audio || audio->replay_len == 0) return 0;
    pthread_mutex_lock(&audio->lock);

    SampleVoice *sv = &audio->replay_voice;
    sv->active = 1;
    sv->loop = loop;
    sv->position = 0.0;
    sv->speed = (freq / base_freq) * ((double)audio->replay_rate / (double)audio->actual_rate);
    sv->volume = volume;
    sv->pan = pan;
    sv->fade = 0.0;
    sv->fade_target = 1.0;
    sv->id = audio->sample_next_id++;

    pthread_mutex_unlock(&audio->lock);
    return sv->id;
}

void audio_sample_kill(ACAudio *audio, uint64_t id, double fade) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++) {
        if (audio->sample_voices[i].active && audio->sample_voices[i].id == id) {
            if (fade <= 0.001) {
                audio->sample_voices[i].active = 0;
            } else {
                audio->sample_voices[i].fade_target = 0.0;
            }
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

void audio_replay_kill(ACAudio *audio, uint64_t id, double fade) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    SampleVoice *sv = &audio->replay_voice;
    if (sv->active && sv->id == id) {
        if (fade <= 0.001) sv->active = 0;
        else sv->fade_target = 0.0;
    }
    pthread_mutex_unlock(&audio->lock);
}

void audio_sample_update(ACAudio *audio, uint64_t id, double freq,
                         double base_freq, double volume, double pan) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++) {
        SampleVoice *sv = &audio->sample_voices[i];
        if (sv->active && sv->id == id) {
            if (freq > 0 && base_freq > 0)
                sv->speed = (freq / base_freq) * ((double)audio->sample_rate / (double)audio->actual_rate);
            if (volume >= 0) sv->volume = volume;
            if (pan > -2) sv->pan = pan;
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

void audio_replay_update(ACAudio *audio, uint64_t id, double freq,
                         double base_freq, double volume, double pan) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    SampleVoice *sv = &audio->replay_voice;
    if (sv->active && sv->id == id) {
        if (freq > 0 && base_freq > 0)
            sv->speed = (freq / base_freq) * ((double)audio->replay_rate / (double)audio->actual_rate);
        if (volume >= 0) sv->volume = volume;
        if (pan > -2) sv->pan = pan;
    }
    pthread_mutex_unlock(&audio->lock);
}

// Read current Master mixer volume as 0-100 percentage
static int read_system_volume_card(int card) {
    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) < 0) return -1;
    char card_name[16];
    snprintf(card_name, sizeof(card_name), "hw:%d", card);
    snd_mixer_attach(mixer, card_name);
    snd_mixer_selem_register(mixer, NULL, NULL);
    snd_mixer_load(mixer);

    int pct = -1;
    snd_mixer_elem_t *elem;
    for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
        if (!snd_mixer_selem_is_active(elem)) continue;
        if (strcasecmp(snd_mixer_selem_get_name(elem), "Master") != 0) continue;
        if (snd_mixer_selem_has_playback_volume(elem)) {
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            if (max > min) pct = (int)((cur - min) * 100 / (max - min));
        }
        break;
    }
    snd_mixer_close(mixer);
    return pct;
}

static int muted = 0;
static long pre_mute_volume = -1;

// Unmute all playback switches in the mixer
static void unmute_all_switches(snd_mixer_t *mixer) {
    snd_mixer_elem_t *elem;
    for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
        if (!snd_mixer_selem_is_active(elem)) continue;
        if (snd_mixer_selem_has_playback_switch(elem))
            snd_mixer_selem_set_playback_switch_all(elem, 1);
    }
}

void audio_volume_adjust(ACAudio *audio, int delta) {
    if (!audio || !audio->pcm) return;

    char card_name[16];
    snprintf(card_name, sizeof(card_name), "hw:%d", audio->card_index);

    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) < 0) return;
    snd_mixer_attach(mixer, card_name);
    snd_mixer_selem_register(mixer, NULL, NULL);
    snd_mixer_load(mixer);

    // Adjust ALL playback volume elements — on Realtek ALC codecs,
    // Master controls digital gain, Speaker/Headphone control the amplifier.
    // Both need to be set for audible volume change.
    const char *try_names[] = {"Master", "Speaker", "Headphone", "PCM", NULL};
    int adjusted = 0;
    for (int n = 0; try_names[n]; n++) {
        snd_mixer_elem_t *elem = NULL;
        for (snd_mixer_elem_t *e = snd_mixer_first_elem(mixer); e; e = snd_mixer_elem_next(e)) {
            if (!snd_mixer_selem_is_active(e)) continue;
            const char *name = snd_mixer_selem_get_name(e);
            if (strcasecmp(name, try_names[n]) == 0 && snd_mixer_selem_has_playback_volume(e))
                { elem = e; break; }
        }
        if (!elem) continue;

        if (delta == 0) {
            // Toggle mute
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            if (!muted) {
                pre_mute_volume = cur;
                snd_mixer_selem_set_playback_volume_all(elem, min);
            } else {
                long restore = (pre_mute_volume > min) ? pre_mute_volume : max * 80 / 100;
                snd_mixer_selem_set_playback_volume_all(elem, restore);
            }
            ac_log("[audio] volume: mute toggle '%s' on %s\n", try_names[n], card_name);
        } else {
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            long step = (max - min) * 5 / 100;
            if (step < 1) step = 1;
            long newvol = cur + step * delta;
            if (newvol < min) newvol = min;
            if (newvol > max) newvol = max;
            snd_mixer_selem_set_playback_volume_all(elem, newvol);
            ac_log("[audio] volume: '%s' %ld→%ld (range %ld-%ld)\n", try_names[n], cur, newvol, min, max);
        }
        adjusted++;
    }
    if (delta == 0) { muted = !muted; }
    if (adjusted) {
        unmute_all_switches(mixer);
        if (delta != 0) muted = 0;
    } else {
        // No elements found — log what's available
        ac_log("[audio] volume: no playback elements on %s. Available:\n", card_name);
        for (snd_mixer_elem_t *e = snd_mixer_first_elem(mixer); e; e = snd_mixer_elem_next(e))
            ac_log("[audio]   %s%s\n", snd_mixer_selem_get_name(e),
                    snd_mixer_selem_has_playback_volume(e) ? " [vol]" : "");
    }
    snd_mixer_close(mixer);

    // Update cached system volume
    audio->system_volume = muted ? 0 : read_system_volume_card(audio->card_index);
}

void audio_boot_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    // Two-tone "doo-dah" — distinct from old single ping (OTA test marker)
    audio_synth(audio, WAVE_SINE, 660.0, 0.12, 0.8, 0.002, 0.08, -0.15);  // E5
    usleep(80000);
    audio_synth(audio, WAVE_SINE, 990.0, 0.15, 0.9, 0.002, 0.10,  0.15);  // B5
}

// Prewarm: play a near-silent note so ALSA buffers are filled and ready
void audio_prewarm(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_SINE, 440.0, 0.05, 0.001, 0.001, 0.04, 0.0);
}

void audio_ready_melody(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    // Quick ascending 3-note toot: C5 → E5 → G5 (major triad) at full volume
    audio_synth(audio, WAVE_TRIANGLE, 523.25, 0.15, 0.7, 0.003, 0.10, -0.2);  // C5
    usleep(60000); // 60ms gap
    audio_synth(audio, WAVE_TRIANGLE, 659.25, 0.15, 0.7, 0.003, 0.10,  0.0);  // E5
    usleep(60000);
    audio_synth(audio, WAVE_TRIANGLE, 783.99, 0.20, 0.8, 0.003, 0.14,  0.2);  // G5
}

void audio_shutdown_sound(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    // Descending 3-note chime: G5 → E5 → C5 at full volume
    audio_synth(audio, WAVE_TRIANGLE, 783.99, 0.15, 0.7, 0.003, 0.10,  0.2);  // G5
    usleep(60000);
    audio_synth(audio, WAVE_TRIANGLE, 659.25, 0.15, 0.7, 0.003, 0.10,  0.0);  // E5
    usleep(60000);
    audio_synth(audio, WAVE_TRIANGLE, 523.25, 0.20, 0.8, 0.003, 0.14, -0.2);  // C5
    // Wait for notes to finish playing before shutdown
    usleep(250000);
}

// Save sample buffer to disk as raw floats with a small header
// Format: [uint32_t sample_rate] [uint32_t sample_len] [float * sample_len]
int audio_sample_save(ACAudio *audio, const char *path) {
    if (!audio || !audio->sample_buf || audio->sample_len <= 0) return -1;
    FILE *f = fopen(path, "wb");
    if (!f) return -1;
    uint32_t rate = (uint32_t)audio->sample_rate;
    uint32_t len = (uint32_t)audio->sample_len;
    fwrite(&rate, sizeof(rate), 1, f);
    fwrite(&len, sizeof(len), 1, f);
    fwrite(audio->sample_buf, sizeof(float), len, f);
    fclose(f);
    sync();
    return (int)len;
}

// Load sample buffer from disk
int audio_sample_load(ACAudio *audio, const char *path) {
    if (!audio || !audio->sample_buf) return -1;
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    uint32_t rate, len;
    if (fread(&rate, sizeof(rate), 1, f) != 1 ||
        fread(&len, sizeof(len), 1, f) != 1) {
        fclose(f);
        return -1;
    }
    if (len > (uint32_t)audio->sample_max_len) len = (uint32_t)audio->sample_max_len;
    if (fread(audio->sample_buf, sizeof(float), len, f) != len) {
        fclose(f);
        return -1;
    }
    fclose(f);
    audio->sample_len = (int)len;
    audio->sample_rate = (int)rate;
    return (int)len;
}

// --- DJ deck API ---

int audio_deck_load(ACAudio *audio, int deck, const char *path) {
    if (!audio || deck < 0 || deck >= AUDIO_MAX_DECKS) return -1;
    ACDeck *dk = &audio->decks[deck];

    // Create decoder if needed
    if (!dk->decoder) {
        dk->decoder = deck_decoder_create(audio->actual_rate);
        if (!dk->decoder) return -1;
    }

    dk->playing = 0;
    dk->active = 0;
    int ret = deck_decoder_load(dk->decoder, path);
    if (ret == 0) {
        dk->active = 1;
        // Generate waveform peaks for visualization (decoded in background thread)
        deck_decoder_generate_peaks(dk->decoder, 1024);
    }
    return ret;
}

void audio_deck_play(ACAudio *audio, int deck) {
    if (!audio || deck < 0 || deck >= AUDIO_MAX_DECKS) return;
    ACDeck *dk = &audio->decks[deck];
    if (!dk->active || !dk->decoder) return;
    dk->playing = 1;
    deck_decoder_play(dk->decoder);
}

void audio_deck_pause(ACAudio *audio, int deck) {
    if (!audio || deck < 0 || deck >= AUDIO_MAX_DECKS) return;
    ACDeck *dk = &audio->decks[deck];
    if (!dk->decoder) return;
    dk->playing = 0;
    deck_decoder_pause(dk->decoder);
}

void audio_deck_seek(ACAudio *audio, int deck, double seconds) {
    if (!audio || deck < 0 || deck >= AUDIO_MAX_DECKS) return;
    ACDeck *dk = &audio->decks[deck];
    if (!dk->active || !dk->decoder) return;
    deck_decoder_seek(dk->decoder, seconds);
}

void audio_deck_set_speed(ACAudio *audio, int deck, double speed) {
    if (!audio || deck < 0 || deck >= AUDIO_MAX_DECKS) return;
    ACDeck *dk = &audio->decks[deck];
    if (!dk->decoder) return;
    deck_decoder_set_speed(dk->decoder, speed);
}

void audio_deck_set_volume(ACAudio *audio, int deck, float vol) {
    if (!audio || deck < 0 || deck >= AUDIO_MAX_DECKS) return;
    if (vol < 0.0f) vol = 0.0f;
    if (vol > 1.0f) vol = 1.0f;
    audio->decks[deck].volume = vol;
}

void audio_deck_set_crossfader(ACAudio *audio, float value) {
    if (!audio) return;
    if (value < 0.0f) value = 0.0f;
    if (value > 1.0f) value = 1.0f;
    audio->crossfader = value;
}

void audio_deck_set_master_volume(ACAudio *audio, float value) {
    if (!audio) return;
    if (value < 0.0f) value = 0.0f;
    if (value > 1.0f) value = 1.0f;
    audio->deck_master_volume = value;
}

void audio_destroy(ACAudio *audio) {
    if (!audio) return;
    audio->running = 0;
    audio_mic_close(audio);
    // Destroy DJ decks
    for (int d = 0; d < AUDIO_MAX_DECKS; d++) {
        if (audio->decks[d].decoder) {
            deck_decoder_destroy(audio->decks[d].decoder);
            audio->decks[d].decoder = NULL;
        }
    }
    if (audio->pcm) {
        pthread_join(audio->thread, NULL);
        snd_pcm_close((snd_pcm_t *)audio->pcm);
    }
    if (audio->hdmi_pcm) snd_pcm_close((snd_pcm_t *)audio->hdmi_pcm);
    free(audio->room_buf_l);
    free(audio->room_buf_r);
    free(audio->sample_buf);
    free(audio->sample_buf_back);
    free(audio->mic_ring);
    free(audio->replay_buf);
    free(audio->replay_buf_back);
    free(audio->output_history_buf);
    free(audio->tts_buf);
    pthread_mutex_destroy(&audio->lock);
    free(audio);
}
