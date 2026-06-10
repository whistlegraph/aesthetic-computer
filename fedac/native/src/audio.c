// audio.c — ALSA sound engine for ac-native
// Dedicated audio thread with multi-voice synthesis, envelopes, and effects.

#define _GNU_SOURCE  // pthread_setaffinity_np, CPU_SET, cpu_set_t

#include "audio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <limits.h>
#include <dirent.h>
#include <sys/stat.h>  // S_ISDIR for piano_bank_dir() probe
#include <unistd.h>
#include <sched.h>
#include <alsa/asoundlib.h>
#include <alsa/use-case.h>

// Defined in ac-native.c — writes to USB log and stderr.
extern void ac_log(const char *fmt, ...);

// Forward declarations
static int read_system_volume_card(int card);

// qsort comparator for AC_LATENCY_BENCH percentile computation.
static int bench_cmp_long(const void *a, const void *b) {
    long la = *(const long *)a, lb = *(const long *)b;
    return (la > lb) - (la < lb);
}

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

// ============================================================
// Sine wavetable — phase-increment lookup (GM synthesis library)
// ============================================================
// The repo rule (MEMORY.md "long sine phase-increment not sin(TAU*f*t)")
// is to advance a phase register and read a wavetable, never call sin()
// per sample for sustained tones. The modal-additive GM piano runs up to
// 12 partials per voice × 32 voices, so a table read per partial is far
// cheaper than 12 libm sin() calls. The dossier refers to this as wt_sin.
// Linear-interpolated, single quadrant-symmetric table built once at init.
#define WT_SIN_SIZE 4096
static float wt_sin_table[WT_SIN_SIZE + 1];  // +1 guard for interp wrap
static int   wt_sin_ready = 0;

static void wt_sin_init(void) {
    if (wt_sin_ready) return;
    for (int i = 0; i <= WT_SIN_SIZE; i++) {
        wt_sin_table[i] = (float)sin(2.0 * M_PI * (double)i / (double)WT_SIN_SIZE);
    }
    wt_sin_ready = 1;
}

// Read the sine wavetable at a normalized phase in [0,1). Wraps any phase.
static inline double wt_sin(double phase) {
    phase -= (double)(int)phase;          // fractional part
    if (phase < 0.0) phase += 1.0;
    double fpos = phase * (double)WT_SIN_SIZE;
    int    i0 = (int)fpos;
    double f = fpos - (double)i0;
    return (double)wt_sin_table[i0] * (1.0 - f)
         + (double)wt_sin_table[i0 + 1] * f;
}

// ============================================================
// Bounded per-note stochasticism (docs/gm-synthesis/00-stochasticism.md)
// ============================================================
// All draws come from the voice's per-trigger noise_seed (seeded in
// audio_synth from next_id). Call these ONCE at note-on and bake the
// result into voice params — never per-sample (phase-increment rule).
// The default 0.6 places the amplitude lever inside the percussion
// ±10-25% band; the pitch lever is hard-capped at ±6 cents.

// Global "organic" amount: 0 = bit-identical, 1 = max tasteful spread.
static double g_organic_amount = 0.6;

// Uniform [0,1) from the voice PRNG.
static inline double voice_rand_unit(ACVoice *v) {
    return (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
}

// Bipolar [-1,1] from the voice PRNG. Workhorse for every jitter lever.
static inline double voice_rand_bipolar(ACVoice *v) {
    return voice_rand_unit(v) * 2.0 - 1.0;
}

// Cents → frequency ratio. 1200 cents = 1 octave. cents_to_ratio(0)==1.0.
static inline double cents_to_ratio(double cents) {
    return pow(2.0, cents / 1200.0);
}

// Bounded multiplicative jitter around `center` by ±`frac` (the percussion
// `rj` idiom): center * (1 ± frac*organic*mul*u). Use for amp, decay,
// attack, cutoff, FM index. Caller picks `frac` from the §4 table.
static inline double voice_jitter(ACVoice *v, double center,
                                  double frac, double mul) {
    double u = voice_rand_bipolar(v);
    return center * (1.0 + frac * g_organic_amount * mul * u);
}

// Bounded pitch detune in cents → ratio, HARD-CAPPED at ±6 cents regardless
// of organic/mul so perceived pitch never moves audibly (invariant §4.1).
#define ORGANIC_MAX_CENTS 6.0
static inline double voice_detune(ACVoice *v, double freq,
                                  double spread_cents, double mul) {
    double cents = spread_cents * g_organic_amount * mul * voice_rand_bipolar(v);
    if (cents >  ORGANIC_MAX_CENTS) cents =  ORGANIC_MAX_CENTS;
    if (cents < -ORGANIC_MAX_CENTS) cents = -ORGANIC_MAX_CENTS;
    return freq * cents_to_ratio(cents);
}

// Random start phase [0,1) for an additive/modal partial. Decorrelates
// stacked same-notes so they don't phase-cancel. Free + identity-safe.
static inline double voice_rand_phase(ACVoice *v) {
    return voice_rand_unit(v);
}

// Small bounded pan offset (±0.05 spread), added to the voice's base pan.
static inline double voice_pan_jitter(ACVoice *v, double base_pan, double mul) {
    double off = 0.05 * g_organic_amount * mul * voice_rand_bipolar(v);
    return clampd(base_pan + off, -1.0, 1.0);
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
    // Allow the full notepat pitch range — C1 ≈ 33Hz, so clamp at 30Hz
    // so we don't lose the bottom octave. On sample rates where SR/freq
    // exceeds the bore buffer (BORE_N=2048, fine at 48kHz; caps around
    // 94Hz at 192kHz), the bore_delay clampd below pins the delay to the
    // buffer size — the worst case is that very low notes play slightly
    // sharper than requested on 192kHz hardware. Still better than the
    // previous 110Hz hard-clamp which silenced every low octave.
    double freq = clampd(v->frequency, 30.0, sample_rate * 0.20);
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
// Harp synthesis — Karplus-Strong plucked string
// ============================================================
//
// The canonical digital plucked-string model. Parallels the whistle's
// waveguide flute but for a *struck/plucked* resonator:
//
//   pluck noise ──► delay line (N = sr/freq) ──┬──► out
//                        ▲                     │
//                        │                     ▼
//                        └── × S ◄── avg[x,x₋₁] (H(z) = 0.5 + 0.5·z⁻¹)
//
// References:
//   [1] Karplus, K. & Strong, A. (1983). "Digital Synthesis of Plucked-
//       String and Drum Timbres," Computer Music Journal 7(2), 43-55.
//       The original algorithm: delay line seeded with noise, two-point
//       moving-average filter in the feedback loop.
//   [2] Jaffe, D.A. & Smith, J.O. (1983). "Extensions of the Karplus-
//       Strong Plucked-String Algorithm," Computer Music Journal 7(2),
//       56-69. Introduces the stretch factor S for decay control,
//       pitch tuning, dynamics, and brightness.
//   [3] Smith, J.O. "Physical Audio Signal Processing," CCRMA Stanford
//       (online book) — https://ccrma.stanford.edu/~jos/pasp/Karplus_Strong_Algorithm.html
//       Later showed K-S is a special case of digital waveguide modeling.
//
// Algorithm per sample:
//   1. Read from the delay line at (w − N) — one wavelength behind.
//   2. Apply the two-point moving-average damping filter:
//        filtered = 0.5 · (delayed + previous_delayed)
//      Unity DC gain, cosine rolloff; higher harmonics decay faster
//      than the fundamental — the spectral signature of real strings.
//   3. Multiply by stretch factor S (<1) to control overall decay time.
//      (Jaffe & Smith 1983: stretch factor decouples decay from N so
//      high and low notes have usable ringing.)
//   4. Write back to the delay line.
//
// Initial pluck: the delay line is pre-filled with a single wavelength
// of pre-smoothed white noise. Smoothing the noise once before injection
// softens the initial transient (less "harsh pluck," more "nylon-ish").
static inline double generate_harp_sample(ACVoice *v, double sample_rate) {
    double env = compute_envelope(v);

    double freq = clampd(v->frequency, 50.0, sample_rate * 0.20);
    double string_delay = sample_rate / freq;
    const int STRING_N = 2048;
    if (string_delay > (double)(STRING_N - 2)) string_delay = (double)(STRING_N - 2);
    if (string_delay < 2.0) string_delay = 2.0;

    // Read delayed sample (one wavelength ago) with fractional interpolation.
    // Reuses whistle_bore_buf since a voice is one wave type at a time.
    double delayed = whistle_frac_read(v->whistle_bore_buf, STRING_N,
                                       v->whistle_bore_w, string_delay);

    // Two-point moving-average damping filter (Karplus & Strong 1983).
    // y[n] = 0.5 · (x[n] + x[n−1]). Zero multiplications in the original —
    // a shift-and-add. We keep the 0.5 constant for clarity.
    double filtered = 0.5 * (delayed + v->harp_lp1);
    v->harp_lp1 = delayed;

    // Stretch factor S (Jaffe-Smith EKS). S < 1 makes the circulating
    // pattern decay exponentially. With the two-point LPF ≈ unity at DC,
    // the per-cycle loss is dominated by S: amplitude ≈ S^(f·t) per second.
    // S = 0.9985 gives T60 ≈ 15s at 440Hz — long sustain like a real
    // pedal harp letting the string ring out.
    //
    // "Short pluck" variant — triggered by a FINITE SHORT duration on the
    // voice. Notepat uses duration=Infinity for sustained harp notes and
    // duration≈0.4 for Shift+letter staccato plucks. The previous
    // implementation keyed on decay<0.2 but that collided with the
    // default decay mode (0.1s), so every harp press got short-stretch
    // and shift did nothing audible.
    double stretch = (!isinf(v->duration) && v->duration > 0.0 && v->duration < 1.0)
                     ? 0.990 : 0.9985;
    double decayed = filtered * stretch;

    // Write back to close the delay loop.
    v->whistle_bore_buf[v->whistle_bore_w] = (float)decayed;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % STRING_N;

    // Output gain — Karplus-Strong's circulating amplitude is heavily
    // attenuated by the LPF + stretch each cycle, so raw output is much
    // quieter than sine/square at the same `volume`. Boost by 2.5× so a
    // plucked note feels comparable in loudness to other wave types.
    // Envelope still controls key-up fade; attack is instantaneous (the
    // initial noise burst IS the pluck).
    return 2.5 * decayed * env;
}

// ============================================================
// Piano synthesis — modal additive grand piano with stretched partials
// ============================================================
//
// Algorithm (Bank/Välimäki modal-additive school, simplified for embedded
// per-sample synthesis at 192kHz). For each note we run ~10 sine partials
// at stretched-harmonic frequencies:
//
//   f_n = n · f0 · sqrt(1 + B · n²)        (Fletcher-Rossing eq. 12.12)
//
// where B is the inharmonicity coefficient — for a real grand B ranges
// from ~5e-5 at A0 (long bass strings) to ~5e-3 at C8 (short, stiff
// treble). We scale B with f0 so high notes are noticeably stretched and
// bass notes stay nearly harmonic.
//
// The signature "ringing" piano character comes from this stretching:
// the 8th partial isn't at 8·f0, it's a few cents sharper. When the
// partials sound together with their natural beating, the ear hears a
// real grand piano instead of a pure-harmonic sawtooth-ish "organ."
//
// Each partial decays exponentially with its own T60 (high partials die
// faster — measured behavior of struck strings: damping ~ω². See Weinreich
// 1977, "Coupled piano strings"). A real grand decays slowly (T60 ~ 8s
// for the fundamental of A4), so the partial decays continue as long as
// the voice is held.
//
// Three slightly mistuned "phantom" fundamental partials at f0±~0.6 Hz
// produce the inter-string beating of a 3-string unison choir without
// the cost of actually running 3 full string models. The beating is
// what makes a piano sound like a piano and not a synth bell.
//
// Hammer thump: a short white-noise burst, low-pass filtered through a
// 1-pole LPF (~3kHz cutoff) with ~5ms exponential decay. This is the
// "felt + wood + soundboard" attack transient — Smith & Van Duyne's
// "commuted synthesis" idea, collapsed into a cheap excitation since we
// don't model the soundboard explicitly.
//
// References embedded above the piano fields in audio.h.
// ============================================================
// Piano sample bank — Salamander Grand Piano V3 (CC0)
// ============================================================
// Loaded once at audio_init from /samples/piano/<midi>.raw. Each .raw
// file is a header-less stream of float32 mono samples at 48 kHz.
// Filename convention: "<midi>.raw" where MIDI is the anchor pitch
// (60 = C4 = 261.63 Hz). The pre-build script in
// fedac/native/scripts/prep-piano-samples decimates the full SFZ to
// these anchor pitches every 3 semitones (matching Salamander's own
// anchor density: C, D#, F#, A across each octave).
//
// Voice playback: pick the nearest anchor by MIDI distance, set step =
// 2^((target_midi - anchor_midi)/12) and read with fractional linear
// interp. Up to ±1.5 semitones from any anchor → minor timbral artifact
// from pitch shifting, well within psychoacoustic acceptance.
typedef struct {
    int    midi;       // anchor MIDI note number
    int    len;        // sample count
    float *data;       // mono float32, sample_rate = AUDIO_SAMPLE_RATE
} PianoSample;
#define PIANO_BANK_MAX 64
static PianoSample piano_bank[PIANO_BANK_MAX];
static int         piano_bank_count = 0;

static const char *piano_bank_dir(void) {
    // Built into the initramfs by build-and-flash-initramfs.sh. Falls
    // back to a /mnt path so the same bank can live on the USB data
    // partition for OTA-2-style large-bank installations.
    static const char *paths[] = {
        "/samples/piano",
        "/mnt/samples/piano",
        NULL
    };
    for (int i = 0; paths[i]; i++) {
        struct stat st;
        if (stat(paths[i], &st) == 0 && S_ISDIR(st.st_mode)) return paths[i];
    }
    return NULL;
}

// Parse "<midi>.raw" → midi int. Returns -1 on parse failure.
static int parse_piano_filename(const char *name) {
    int n = 0;
    const char *p = name;
    while (*p >= '0' && *p <= '9') { n = n * 10 + (*p - '0'); p++; }
    if (p == name) return -1;
    if (strcmp(p, ".raw") != 0) return -1;
    if (n < 0 || n > 127) return -1;
    return n;
}

void load_piano_bank(void) {
    piano_bank_count = 0;
    const char *dir_path = piano_bank_dir();
    if (!dir_path) {
        ac_log("[piano-bank] no samples dir found — piano will be silent\n");
        return;
    }
    DIR *dir = opendir(dir_path);
    if (!dir) {
        ac_log("[piano-bank] opendir failed: %s\n", dir_path);
        return;
    }
    struct dirent *ent;
    while ((ent = readdir(dir)) != NULL && piano_bank_count < PIANO_BANK_MAX) {
        if (ent->d_name[0] == '.') continue;
        int midi = parse_piano_filename(ent->d_name);
        if (midi < 0) continue;
        char path[512];
        snprintf(path, sizeof(path), "%s/%s", dir_path, ent->d_name);
        FILE *fp = fopen(path, "rb");
        if (!fp) continue;
        fseek(fp, 0, SEEK_END);
        long sz = ftell(fp);
        rewind(fp);
        if (sz <= 0 || sz % sizeof(float) != 0) {
            fclose(fp); continue;
        }
        int sample_count = (int)(sz / sizeof(float));
        float *buf = malloc((size_t)sz);
        if (!buf) { fclose(fp); continue; }
        size_t rd = fread(buf, 1, (size_t)sz, fp);
        fclose(fp);
        if (rd != (size_t)sz) { free(buf); continue; }
        piano_bank[piano_bank_count].midi = midi;
        piano_bank[piano_bank_count].len  = sample_count;
        piano_bank[piano_bank_count].data = buf;
        piano_bank_count++;
    }
    closedir(dir);
    ac_log("[piano-bank] loaded %d samples from %s\n",
           piano_bank_count, dir_path);
}

// Pick the bank entry closest to the target MIDI note.
static const PianoSample *pick_piano_anchor(int target_midi) {
    if (piano_bank_count == 0) return NULL;
    const PianoSample *best = &piano_bank[0];
    int best_dist = abs(target_midi - best->midi);
    for (int i = 1; i < piano_bank_count; i++) {
        int d = abs(target_midi - piano_bank[i].midi);
        if (d < best_dist) { best_dist = d; best = &piano_bank[i]; }
    }
    return best;
}

static inline double generate_piano_sample(ACVoice *v, double sample_rate) {
    (void)sample_rate;
    double env = compute_envelope(v);

    if (!v->piano_sample_data || v->piano_sample_len <= 0) return 0.0;

    double pos = v->piano_sample_pos;
    int idx = (int)pos;
    if (idx >= v->piano_sample_len - 1) {
        // Sample exhausted — note has run its natural decay length.
        return 0.0;
    }
    double frac = pos - (double)idx;
    double s0 = (double)v->piano_sample_data[idx];
    double s1 = (double)v->piano_sample_data[idx + 1];
    double s  = (1.0 - frac) * s0 + frac * s1;

    v->piano_sample_pos = pos + v->piano_sample_step;

    return s * v->piano_sample_amp * env;
}

// (Old K-S banded waveguide piano + earlier modal-additive synth both
// removed in favor of the Salamander sample bank above. Search the git
// history for "Karplus-Strong" or "Bank/Välimäki" if you need to compare.)

// ============================================================
// GM synthesis library — Family 1: Piano (GM programs 1-8)
// docs/gm-synthesis/01-piano-mallet-organ-guitar.md
// docs/gm-synthesis/00-stochasticism.md
// ============================================================
// First vertical slice of the algorithmic General-MIDI voice set. Three
// engines cover the eight Piano-family programs, selected per program by
// gm_voice_init() from the data table below (mirroring the gun_presets[]
// const-table pattern — timbre lives in DATA, not code, so families 2-16
// extend it the same way):
//
//   GM 1-4 Acoustic/Bright/Electric-grand/Honky-tonk → WAVE_GMPIANO
//       Modal additive with inharmonic stretched partials,
//       f_n = n·f0·sqrt(1 + B·n²)  (Fletcher & Rossing 1998, eq. 12.12),
//       per-partial exponential decay, a hammer-thump noise burst, and
//       (honky-tonk) a second detuned string copy.
//   GM 5-6 Electric Piano 1/2 (Rhodes tine / Wurli reed) → WAVE_EPIANO
//       2-operator Chowning FM with an exponentially-decaying index plus a
//       high-ratio attack "tine" operator, then an asymmetric-pickup tanh.
//       Chowning (1973), JAES 21(7); Pfeifle & Bader (2017), DAFx-17.
//   GM 7-8 Harpsichord / Clavi → WAVE_PLUCK
//       Extended Karplus-Strong (Jaffe & Smith 1983, CMJ 7(2)) with a
//       pluck-position comb, loop-damping LPF, stretch, and (clavi) a
//       pickup-bite waveshaper. String delay line reuses whistle_bore_buf.
//
// Bounded note-on stochasticism (dossier 00) is applied ONCE at note-on:
// per-partial amp/decay jitter, pitch detune (hard-capped ±6 cents), random
// start phase, attack jitter. Family mul = 0.8 (pianos), 0.7 (FM), 0.9
// (plucked) per dossier 00 §6.
typedef struct {
    WaveType wave;          // which engine renders this program
    // -- Modal acoustic-piano params (WAVE_GMPIANO) --
    int    partials;        // partial count (6-12)
    double B;               // inharmonicity coefficient (treble grows it)
    double partial_tilt;    // >1 boosts upper-partial amps (bright pianos)
    double tilt_from;       // first partial index the tilt applies to
    double tau0;            // fundamental T60 seconds (treble partials scale down)
    double hammer_amp;      // hammer-thump mix gain
    double hammer_ms;       // hammer-thump exp decay (ms)
    double dual_cents;      // honky-tonk 2nd-copy detune (cents); 0 = single
    double drive;           // per-voice tanh drive (electric grand); 0 = clean
    // -- FM tine/reed params (WAVE_EPIANO) --
    double fm_ratio;        // body modulator : carrier ratio
    double fm_index0;       // initial body modulation index
    double fm_index_ms;     // index decay time const (ms) → mellowing
    double fm_tine_ratio;   // high-ratio attack "tine" operator : carrier
    double fm_tine_index0;  // tine initial index
    double fm_tine_ms;      // tine index decay (ms) — fast, the attack ping
    double fm_pickup;       // asymmetric-pickup tanh bias (even-harmonic growl)
    // -- Extended-KS params (WAVE_PLUCK) --
    double ks_stretch;      // EKS stretch S (<1) — overall decay
    double ks_loop_b;       // loop-LPF coefficient (brightness; lower=brighter)
    double ks_beta;         // pluck position (fraction; near-bridge = nasal)
    double ks_pick;         // pick-position comb mix
    double ks_drive;        // output tanh drive (clavi bite); 0 = clean
} GMProgramParams;

#define GM_PIANO_PROGRAM_COUNT 8
static const GMProgramParams gm_piano_programs[GM_PIANO_PROGRAM_COUNT] = {
    // GM 1 — Acoustic Grand: reference modal voice, full partial map.
    { .wave = WAVE_GMPIANO, .partials = 10, .B = 0.0009, .partial_tilt = 1.0,
      .tilt_from = 0, .tau0 = 9.0, .hammer_amp = 0.18, .hammer_ms = 5.0,
      .dual_cents = 0.0, .drive = 0.0 },
    // GM 2 — Bright Acoustic: boost upper partials, harder/shorter hammer.
    { .wave = WAVE_GMPIANO, .partials = 11, .B = 0.0010, .partial_tilt = 1.45,
      .tilt_from = 4, .tau0 = 9.5, .hammer_amp = 0.24, .hammer_ms = 3.5,
      .dual_cents = 0.0, .drive = 0.0 },
    // GM 3 — Electric Grand (CP-70-ish): fewer cleaner partials, half B,
    // light tanh drive for the "electrified" edge.
    { .wave = WAVE_GMPIANO, .partials = 7, .B = 0.00045, .partial_tilt = 1.1,
      .tilt_from = 1, .tau0 = 7.0, .hammer_amp = 0.10, .hammer_ms = 4.0,
      .dual_cents = 0.0, .drive = 0.10 },
    // GM 4 — Honky-tonk: dual detuned string copies (~14 cents), more clack.
    { .wave = WAVE_GMPIANO, .partials = 9, .B = 0.0011, .partial_tilt = 1.15,
      .tilt_from = 2, .tau0 = 6.0, .hammer_amp = 0.26, .hammer_ms = 4.5,
      .dual_cents = 14.0, .drive = 0.0 },
    // GM 5 — Electric Piano 1 (Rhodes tine): c:m≈1:1 body + 14:1 tine ping,
    // index decays → mellow bell; mild pickup growl.
    { .wave = WAVE_EPIANO, .fm_ratio = 1.0, .fm_index0 = 1.2, .fm_index_ms = 700.0,
      .fm_tine_ratio = 14.0, .fm_tine_index0 = 0.9, .fm_tine_ms = 18.0,
      .fm_pickup = 0.18 },
    // GM 6 — Electric Piano 2 (Wurli reed): c:m≈1:2, stronger asym pickup,
    // faster decay, hollow/reedy.
    { .wave = WAVE_EPIANO, .fm_ratio = 2.0, .fm_index0 = 1.6, .fm_index_ms = 420.0,
      .fm_tine_ratio = 10.0, .fm_tine_index0 = 0.7, .fm_tine_ms = 14.0,
      .fm_pickup = 0.34 },
    // GM 7 — Harpsichord: very bright EKS, pluck near bridge (β≈0.13),
    // short-ish even decay, the comb is the signature; clean output.
    { .wave = WAVE_PLUCK, .ks_stretch = 0.9965, .ks_loop_b = 0.18,
      .ks_beta = 0.13, .ks_pick = 0.95, .ks_drive = 0.0 },
    // GM 8 — Clavi: pluck very near the end (β≈0.05), thin bright tone, fast
    // decay, tanh pickup bite for the funky electric edge.
    { .wave = WAVE_PLUCK, .ks_stretch = 0.990, .ks_loop_b = 0.12,
      .ks_beta = 0.05, .ks_pick = 0.9, .ks_drive = 0.5 },
};

// Note-on init for one GM Piano-family program (0-based: 0 = Acoustic Grand).
// Selects v->type, fills the per-voice synthesis state from the program row,
// and applies bounded note-on stochasticism (drawn ONCE here from the voice's
// noise_seed). Programs outside 0..7 return -1 so the caller falls back to the
// normal `type` path. `rng` is unused for now (the voice carries its own
// noise_seed) but is accepted so future families can share an external stream.
static int gm_voice_init(ACVoice *v, int program, double freq,
                         double sample_rate, uint32_t *rng) {
    (void)rng;
    if (program < 0 || program >= GM_PIANO_PROGRAM_COUNT) return -1;
    const GMProgramParams *p = &gm_piano_programs[program];
    double sr = sample_rate > 0.0 ? sample_rate : (double)AUDIO_SAMPLE_RATE;
    double f0 = freq < 20.0 ? 20.0 : freq;
    v->gm_program = program;
    v->type = p->wave;

    if (p->wave == WAVE_GMPIANO) {
        // --- Modal additive with inharmonic stretched partials (GM 1-4) ---
        const double mul = 0.8;             // dossier 00 §6: acoustic piano
        int N = p->partials;
        if (N > GM_MAX_PARTIALS) N = GM_MAX_PARTIALS;
        if (N < 1) N = 1;
        v->p_count = N;
        v->gm_dual = (p->dual_cents > 0.0) ? 1 : 0;
        v->gm_drive = p->drive;
        // Normalize so the partial set sums to a sane peak (1/sqrt(N) keeps
        // the additive sum near unity for the velocity-1 case; soft_clip
        // catches any residual peak).
        double norm = 1.0 / sqrt((double)N);
        double sum_check = 0.0;
        for (int k = 0; k < N; k++) {
            int n = k + 1;                  // harmonic number (1-based)
            // Inharmonic stretch f_n = n·f0·sqrt(1 + B·n²).
            double fn = (double)n * f0 * sqrt(1.0 + p->B * (double)(n * n));
            // Per-partial detune (3-string phantom beating) under ±6c cap.
            fn = voice_detune(v, fn, 6.0, mul);
            if (fn > sr * 0.45) fn = sr * 0.45;  // anti-alias guard
            v->p_finc[k] = fn / sr;
            // Base amplitude: 1/n rolloff, optional upper-partial tilt for
            // bright pianos, then ±15% per-partial amp jitter (zero-mean).
            double a = norm / (double)n;
            if (k >= (int)p->tilt_from) a *= p->partial_tilt;
            a = voice_jitter(v, a, 0.15, mul);
            v->p_amp[k] = a;
            sum_check += a;
            // Random start phase decorrelates stacked same-notes (additive
            // tone has no transient to keep coherent).
            v->p_phase[k] = voice_rand_phase(v);
            // Per-partial T60: high partials die first (damping ~ n²),
            // with ±15% decay jitter. dec_mult = exp(-1/(tau·sr)).
            double tau = p->tau0 / (1.0 + 0.6 * (double)(n - 1));
            tau = voice_jitter(v, tau, 0.15, mul);
            if (tau < 0.02) tau = 0.02;
            v->p_dec_mult[k] = exp(-1.0 / (tau * sr));
            // Honky-tonk: second string copy, detuned by dual_cents, with
            // its own random phase. Phase increment only — no extra amp/decay
            // state (shares p_amp[k]/p_dec_mult[k] at render time).
            if (v->gm_dual) {
                double f2 = fn * cents_to_ratio(p->dual_cents);
                if (f2 > sr * 0.45) f2 = sr * 0.45;
                v->p2_finc[k] = f2 / sr;
                v->p2_phase[k] = voice_rand_phase(v);
            }
        }
        (void)sum_check;
        // Hammer thump: short LPF noise burst (set up the envelope; the noise
        // itself is consumed per-sample from noise_seed in the render loop).
        double htau = (p->hammer_ms * 0.001);
        if (htau < 0.0005) htau = 0.0005;
        v->gm_hammer_amp = voice_jitter(v, p->hammer_amp, 0.15, mul);
        v->gm_hammer_env = 1.0;
        v->gm_hammer_dec = exp(-1.0 / (htau * sr));
        v->gm_hammer_lp = 0.0;
    } else if (p->wave == WAVE_EPIANO) {
        // --- FM tine/reed (Chowning), GM 5-6 ---
        const double mul = 0.7;             // dossier 00 §6: electric piano
        // Carrier at f0, body modulator at fm_ratio·f0. Ratio gets a tiny
        // (±2c spread) detune — an FM ratio error IS a sideband detune, so
        // it rides the pitch ceiling, not the amplitude lever.
        double fc = voice_detune(v, f0, 6.0, mul);
        double fm = voice_detune(v, f0 * p->fm_ratio, 2.0, mul);
        double ft = f0 * p->fm_tine_ratio;
        if (fc > sr * 0.45) fc = sr * 0.45;
        v->fm_cphase = 0.0; v->fm_cinc = fc / sr;
        v->fm_mphase = 0.0; v->fm_minc = fm / sr;
        v->fm_tphase = 0.0; v->fm_tinc = ft / sr;
        // Index jitter ≈ the brightness/energy lever (±6%).
        v->fm_index = voice_jitter(v, p->fm_index0, 0.06, mul);
        v->fm_tindex = voice_jitter(v, p->fm_tine_index0, 0.06, mul);
        double idec = p->fm_index_ms * 0.001;  if (idec < 0.001) idec = 0.001;
        double tdec = p->fm_tine_ms * 0.001;   if (tdec < 0.0005) tdec = 0.0005;
        v->fm_index_dec  = exp(-1.0 / (idec * sr));
        v->fm_tindex_dec = exp(-1.0 / (tdec * sr));
        v->fm_pickup_bias = p->fm_pickup;
        v->fm_hp_x1 = 0.0; v->fm_hp_y1 = 0.0;
    } else if (p->wave == WAVE_PLUCK) {
        // --- Extended Karplus-Strong (harpsichord/clavi), GM 7-8 ---
        const double mul = 0.9;             // dossier 00 §6: plucked
        v->harp_lp1 = 0.0;
        v->ks_stretch = p->ks_stretch;
        v->ks_loop_b  = p->ks_loop_b;
        // Pluck position β with ±8% jitter (player doesn't hit the same spot).
        v->ks_beta = voice_jitter(v, p->ks_beta, 0.08, mul);
        v->ks_pick_amt = p->ks_pick;
        v->ks_drive = p->ks_drive;
        // Seed the string delay line with one wavelength of bright noise.
        // The excitation seed (noise_seed, fresh per trigger) IS the per-pluck
        // organic variation. Harpsichord = bright (less pre-smoothing) than
        // the nylon harp; we do a single light smoothing pass.
        memset(v->whistle_bore_buf, 0, sizeof(v->whistle_bore_buf));
        double string_delay = sr / f0;
        const int STRING_N = 2048;
        if (string_delay > (double)(STRING_N - 2)) string_delay = (double)(STRING_N - 2);
        if (string_delay < 2.0) string_delay = 2.0;
        int n = (int)string_delay;
        double last = 0.0;
        for (int i = 0; i < n; i++) {
            double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
            // Light one-pole smoothing (brighter than the harp's 0.5/0.5).
            double filt = 0.75 * white + 0.25 * last;
            last = white;
            v->whistle_bore_buf[i] = (float)filt;
        }
        v->whistle_bore_w = n;
    }
    return 0;
}

// ── GM acoustic-piano render: modal additive + inharmonicity + hammer ──
// (docs/gm-synthesis/01, GM 1-4). Phase-increment wavetable sines only.
static inline double generate_gmpiano_sample(ACVoice *v, double sample_rate) {
    (void)sample_rate;
    double env = compute_envelope(v);
    double s = 0.0;
    int N = v->p_count;
    for (int k = 0; k < N; k++) {
        double a = v->p_amp[k];
        s += a * wt_sin(v->p_phase[k]);
        v->p_phase[k] += v->p_finc[k];
        if (v->p_phase[k] >= 1.0) v->p_phase[k] -= 1.0;
        if (v->gm_dual) {
            // Second detuned string copy (honky-tonk) shares this partial's
            // amplitude; its slightly different frequency beats with the first.
            s += a * wt_sin(v->p2_phase[k]);
            v->p2_phase[k] += v->p2_finc[k];
            if (v->p2_phase[k] >= 1.0) v->p2_phase[k] -= 1.0;
        }
        v->p_amp[k] *= v->p_dec_mult[k];   // exp decay; high partials die first
    }
    if (v->gm_dual) s *= 0.5;              // two copies → keep level normalized
    // Hammer thump: short LPF white-noise burst, exp-decaying envelope.
    if (v->gm_hammer_env > 0.0001) {
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        // 1-pole LPF (~ felt softness) — leak 0.2 of new sample in.
        v->gm_hammer_lp = 0.2 * white + 0.8 * v->gm_hammer_lp;
        s += v->gm_hammer_amp * v->gm_hammer_env * v->gm_hammer_lp;
        v->gm_hammer_env *= v->gm_hammer_dec;
    }
    // Per-voice tanh drive for the "electrified" grand (GM 3); clean for others.
    if (v->gm_drive > 0.0) {
        double pre = 1.0 + 4.0 * v->gm_drive;
        s = tanh(pre * s) * (1.0 / pre) * (1.0 + v->gm_drive);
    }
    return s * env;
}

// ── GM electric-piano render: 2-op FM tine + attack tine + pickup tanh ──
// (docs/gm-synthesis/01, GM 5-6). Chowning FM, wavetable-sine operators.
static inline double generate_epiano_sample(ACVoice *v, double sample_rate) {
    (void)sample_rate;
    double env = compute_envelope(v);
    // Body modulator (index decays → bell-like mellowing) + high-ratio tine
    // operator (fast-decaying index → the metallic attack "ping").
    double bodymod = wt_sin(v->fm_mphase) * v->fm_index;
    double tinemod = wt_sin(v->fm_tphase) * v->fm_tindex;
    double car = wt_sin(v->fm_cphase + bodymod + tinemod);
    v->fm_cphase += v->fm_cinc;  if (v->fm_cphase >= 1.0) v->fm_cphase -= 1.0;
    v->fm_mphase += v->fm_minc;  if (v->fm_mphase >= 1.0) v->fm_mphase -= 1.0;
    v->fm_tphase += v->fm_tinc;  if (v->fm_tphase >= 1.0) v->fm_tphase -= 1.0;
    v->fm_index  *= v->fm_index_dec;
    v->fm_tindex *= v->fm_tindex_dec;
    // Asymmetric pickup nonlinearity — biased tanh injects even harmonics
    // (the Rhodes/Wurli "growl"), then a DC blocker removes the bias the
    // bias term would otherwise pump into the output.
    double x = car;
    if (v->fm_pickup_bias > 0.0) {
        double biased = tanh(x + v->fm_pickup_bias);
        double y = biased - v->fm_hp_x1 + 0.999 * v->fm_hp_y1;  // 1-pole DC block
        v->fm_hp_x1 = biased;
        v->fm_hp_y1 = y;
        x = y;
    }
    return x * env;
}

// ── GM plucked render: extended Karplus-Strong (harpsichord/clavi) ──
// (docs/gm-synthesis/01, GM 7-8). Reuses whistle_bore_buf string delay,
// adds pluck-position comb, loop-damping LPF, stretch, and pickup drive.
static inline double generate_pluck_sample(ACVoice *v, double sample_rate) {
    double env = compute_envelope(v);
    double freq = clampd(v->frequency, 50.0, sample_rate * 0.20);
    double string_delay = sample_rate / freq;
    const int STRING_N = 2048;
    if (string_delay > (double)(STRING_N - 2)) string_delay = (double)(STRING_N - 2);
    if (string_delay < 2.0) string_delay = 2.0;

    // Delayed sample, one wavelength behind (fractional read).
    double delayed = whistle_frac_read(v->whistle_bore_buf, STRING_N,
                                       v->whistle_bore_w, string_delay);
    // Pick-position comb (Jaffe-Smith H_β): subtract a tap at β·N → the
    // bright nasal notches that are the harpsichord/clavi signature.
    double tap_delay = v->ks_beta * string_delay;
    if (tap_delay < 1.0) tap_delay = 1.0;
    double picked = delayed - v->ks_pick_amt *
        whistle_frac_read(v->whistle_bore_buf, STRING_N, v->whistle_bore_w, tap_delay);
    // Loop damping (one-pole LPF; higher ks_loop_b = darker). harp_lp1 holds
    // the filter state across samples.
    double damp = (1.0 - v->ks_loop_b) * picked + v->ks_loop_b * v->harp_lp1;
    v->harp_lp1 = damp;
    // Stretch S (<1) sets overall decay.
    double y = v->ks_stretch * damp;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)y;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % STRING_N;
    // Output bite — clavi pickup waveshaper (clean for harpsichord).
    double out = y;
    if (v->ks_drive > 0.0) {
        double pre = 1.0 + 4.0 * v->ks_drive;
        out = tanh(pre * y) * (1.0 / pre) * (1.0 + v->ks_drive);
    }
    // Same loudness boost as the harp (K-S circulating amplitude is low).
    return 2.5 * out * env;
}

// ============================================================
// One-shot named-buffer bank (zoo / lasers / future kits)
// ============================================================
// Mirrors the piano bank above but indexed by string name and addressed
// per-voice (each concurrent voice can play a different buffer). This
// is what notepat's `zoo` and `lasers` kits route through — they trigger
// pre-recorded one-shot files instead of synthesizing each animal/laser
// from oscillators.
//
// File layout on disk: <bank_root>/<name>.raw — header-less float32
// mono at AUDIO_SAMPLE_RATE. The pre-build scripts
// (scripts/prep-zoo-samples.sh) generate these and check them into the
// repo; build-and-flash-initramfs.sh copies them to /samples/<kit>/.
//
// At load time we scan both /samples/zoo and /samples/lasers (if
// present) and merge into one flat name-keyed table. Names collide at
// peril of last-write-wins — keep them unique across kits.
typedef struct {
    char     name[16];    // lowercase stem, NUL-terminated, max 15 chars
    int      len;         // sample count
    float   *data;        // mono float32 at AUDIO_SAMPLE_RATE
} OneShotSample;
#define ONESHOT_BANK_MAX 64
static OneShotSample oneshot_bank[ONESHOT_BANK_MAX];
static int           oneshot_bank_count = 0;

typedef struct {
    int             active;
    const OneShotSample *sample;  // points into oneshot_bank
    double          position;     // fractional sample index
    double          speed;        // 1.0 = original pitch
    double          volume;
    double          pan;
    double          fade;
    double          fade_target;
    uint64_t        id;
} OneShotVoice;
#define ONESHOT_MAX_VOICES 16
static OneShotVoice oneshot_voices[ONESHOT_MAX_VOICES];
static uint64_t     oneshot_next_id = 1;
static pthread_mutex_t oneshot_lock = PTHREAD_MUTEX_INITIALIZER;

// Lowercase-copy up to dst_max-1 chars. Returns dst.
static char *oneshot_lowercase_copy(char *dst, size_t dst_max, const char *src) {
    size_t i = 0;
    for (; src[i] && i + 1 < dst_max; i++) {
        char c = src[i];
        if (c >= 'A' && c <= 'Z') c = (char)(c - 'A' + 'a');
        dst[i] = c;
    }
    dst[i] = '\0';
    return dst;
}

// Load any *.raw files from `dir_path` into oneshot_bank[]. Stops if
// the bank fills (rare — 64 entries is plenty for two kits).
static void load_oneshot_bank_dir(const char *dir_path) {
    DIR *dir = opendir(dir_path);
    if (!dir) return;
    struct dirent *ent;
    while ((ent = readdir(dir)) != NULL && oneshot_bank_count < ONESHOT_BANK_MAX) {
        if (ent->d_name[0] == '.') continue;
        size_t nlen = strlen(ent->d_name);
        if (nlen < 5 || strcmp(ent->d_name + nlen - 4, ".raw") != 0) continue;

        char stem[32];
        size_t stem_len = nlen - 4;
        if (stem_len >= sizeof(stem)) stem_len = sizeof(stem) - 1;
        memcpy(stem, ent->d_name, stem_len);
        stem[stem_len] = '\0';

        char path[512];
        snprintf(path, sizeof(path), "%s/%s", dir_path, ent->d_name);
        FILE *fp = fopen(path, "rb");
        if (!fp) continue;
        fseek(fp, 0, SEEK_END);
        long sz = ftell(fp);
        rewind(fp);
        if (sz <= 0 || sz % (long)sizeof(float) != 0) {
            fclose(fp); continue;
        }
        int sample_count = (int)(sz / (long)sizeof(float));
        float *buf = malloc((size_t)sz);
        if (!buf) { fclose(fp); continue; }
        size_t rd = fread(buf, 1, (size_t)sz, fp);
        fclose(fp);
        if (rd != (size_t)sz) { free(buf); continue; }

        // If a sample by this name already exists, replace it (lasers
        // overrides zoo if names ever collide). Keeps bank flat.
        char lc_name[16];
        oneshot_lowercase_copy(lc_name, sizeof(lc_name), stem);
        int slot = -1;
        for (int i = 0; i < oneshot_bank_count; i++) {
            if (strcmp(oneshot_bank[i].name, lc_name) == 0) { slot = i; break; }
        }
        if (slot < 0) {
            slot = oneshot_bank_count++;
        } else {
            free(oneshot_bank[slot].data);
        }
        strncpy(oneshot_bank[slot].name, lc_name, sizeof(oneshot_bank[slot].name) - 1);
        oneshot_bank[slot].name[sizeof(oneshot_bank[slot].name) - 1] = '\0';
        oneshot_bank[slot].len  = sample_count;
        oneshot_bank[slot].data = buf;
    }
    closedir(dir);
}

void load_oneshot_bank(void) {
    oneshot_bank_count = 0;
    // Scan known kit dirs. /mnt fallback mirrors the piano bank pattern
    // so an OTA-2-style USB data partition can carry larger sample sets.
    static const char *paths[] = {
        "/samples/zoo",
        "/samples/lasers",
        "/mnt/samples/zoo",
        "/mnt/samples/lasers",
        NULL,
    };
    for (int i = 0; paths[i]; i++) {
        struct stat st;
        if (stat(paths[i], &st) != 0 || !S_ISDIR(st.st_mode)) continue;
        load_oneshot_bank_dir(paths[i]);
    }
    if (oneshot_bank_count == 0) {
        ac_log("[oneshot-bank] no samples found — zoo/lasers will be silent\n");
        return;
    }
    ac_log("[oneshot-bank] loaded %d samples\n", oneshot_bank_count);
}

static const OneShotSample *find_oneshot(const char *name) {
    if (!name || !*name) return NULL;
    char lc[16];
    oneshot_lowercase_copy(lc, sizeof(lc), name);
    for (int i = 0; i < oneshot_bank_count; i++) {
        if (strcmp(oneshot_bank[i].name, lc) == 0) return &oneshot_bank[i];
    }
    return NULL;
}

uint64_t audio_oneshot_play(ACAudio *audio, const char *name,
                            double volume, double pan, double pitch_factor) {
    (void)audio;
    const OneShotSample *s = find_oneshot(name);
    if (!s) {
        ac_log("[oneshot] play '%s' — sample not in bank\n", name ? name : "(null)");
        return 0;
    }
    if (pitch_factor <= 0.0) pitch_factor = 1.0;
    if (volume < 0.0) volume = 0.0;
    if (pan < -1.0) pan = -1.0;
    if (pan >  1.0) pan =  1.0;

    pthread_mutex_lock(&oneshot_lock);
    // Find a free slot, or steal the oldest (lowest id) active voice.
    int slot = -1;
    uint64_t oldest_id = (uint64_t)-1;
    int oldest_slot = 0;
    for (int i = 0; i < ONESHOT_MAX_VOICES; i++) {
        if (!oneshot_voices[i].active) { slot = i; break; }
        if (oneshot_voices[i].id < oldest_id) {
            oldest_id = oneshot_voices[i].id;
            oldest_slot = i;
        }
    }
    if (slot < 0) slot = oldest_slot;

    OneShotVoice *v = &oneshot_voices[slot];
    v->active      = 1;
    v->sample      = s;
    v->position    = 0.0;
    v->speed       = pitch_factor;
    v->volume      = volume;
    v->pan         = pan;
    v->fade        = 1.0;   // no attack ramp for one-shots
    v->fade_target = 1.0;
    v->id          = oneshot_next_id++;
    uint64_t id = v->id;
    pthread_mutex_unlock(&oneshot_lock);
    return id;
}

void audio_oneshot_kill(ACAudio *audio, uint64_t id, double fade) {
    (void)audio;
    if (id == 0) return;
    pthread_mutex_lock(&oneshot_lock);
    for (int i = 0; i < ONESHOT_MAX_VOICES; i++) {
        OneShotVoice *v = &oneshot_voices[i];
        if (!v->active || v->id != id) continue;
        if (fade <= 0.001) {
            v->active = 0;
        } else {
            v->fade_target = 0.0;
        }
        break;
    }
    pthread_mutex_unlock(&oneshot_lock);
}

// Mix all active one-shot voices into the (mix_l, mix_r) bus. Called
// from the audio thread alongside the SampleVoice mixer.
static void mix_oneshot_voices(double rate, double *mix_l, double *mix_r) {
    // 5 ms fade ramp at output rate — matches mix_sample_voice fade speed.
    double fade_speed = 1.0 / (0.005 * rate);
    pthread_mutex_lock(&oneshot_lock);
    for (int i = 0; i < ONESHOT_MAX_VOICES; i++) {
        OneShotVoice *v = &oneshot_voices[i];
        if (!v->active || !v->sample || !v->sample->data || v->sample->len < 2) continue;

        // Fade envelope (release ramp for sustained kills).
        if (v->fade < v->fade_target) {
            v->fade += fade_speed;
            if (v->fade > v->fade_target) v->fade = v->fade_target;
        } else if (v->fade > v->fade_target) {
            v->fade -= fade_speed;
            if (v->fade <= 0.0) { v->fade = 0.0; v->active = 0; continue; }
        }

        int slen = v->sample->len;
        const float *buf = v->sample->data;
        double pos = v->position;
        if (pos < 0.0) pos = 0.0;
        if (pos >= (double)(slen - 1)) {
            v->active = 0;
            continue;
        }
        int p0 = (int)pos;
        int p1 = p0 + 1;
        if (p1 >= slen) p1 = p0;
        double frac = pos - (double)p0;
        double s = (double)buf[p0] * (1.0 - frac) + (double)buf[p1] * frac;

        double vol = v->volume * v->fade;
        // Equal-power-ish pan: SampleVoice uses a 0.6 cosine-approx; mirror
        // that here so zoo voices sit at consistent levels with the rest.
        double l_gain = v->pan <= 0 ? 1.0 : 1.0 - v->pan * 0.6;
        double r_gain = v->pan >= 0 ? 1.0 : 1.0 + v->pan * 0.6;
        *mix_l += s * vol * l_gain;
        *mix_r += s * vol * r_gain;

        v->position += v->speed;
        if (v->position >= (double)(slen - 1)) {
            v->active = 0;
        }
    }
    pthread_mutex_unlock(&oneshot_lock);
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
    // --- GUN_SMG (MP5, L≈225mm) — bright fast crack, full-auto ~1000 RPM
    { .model = GUN_MODEL_CLASSIC, .master_amp = 0.95,
      .click_amp = 0.55, .click_decay_ms = 0.4,
      .crack_amp = 0.85, .crack_decay_ms = 5.0,  .crack_fc = 4200, .crack_q = 2.5,
      .boom_amp  = 0.40, .boom_freq_start = 200, .boom_freq_end = 60,
      .boom_pitch_decay_ms = 10, .boom_amp_decay_ms = 40,
      .tail_amp  = 0.28, .tail_attack_ms = 0,    .tail_decay_ms = 80,
      .tail_fc   = 1200, .tail_q = 0.7,
      .sustain_fire = 1, .retrig_period_ms = 60,  // 1000 RPM
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
    // Master amp tamed so the first shot doesn't stand out from the
    // sustained burst (sustain-fire weapons also start their envelopes
    // at the average jitter level — see gun_init_voice).
    { .model = GUN_MODEL_CLASSIC, .master_amp = 0.9,
      .click_amp = 0.55, .click_decay_ms = 0.5,
      .crack_amp = 0.85, .crack_decay_ms = 7.0,  .crack_fc = 3500, .crack_q = 2.6,
      .boom_amp  = 0.65, .boom_freq_start = 250, .boom_freq_end = 48,
      .boom_pitch_decay_ms = 16, .boom_amp_decay_ms = 75,
      .tail_amp  = 0.40, .tail_attack_ms = 0,    .tail_decay_ms = 160,
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
    // Sustain-fire weapons (SMG/LMG) start at the same gentler level
    // their internal retrigger uses (avg jitter ≈ 0.95) so the first
    // shot blends with the rapid-fire stream instead of standing out.
    v->gun_pressure_env = p->sustain_fire ? 0.92 : 1.0;
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
        v->gun_boom_env = (p->boom_amp > 0.0) ? (p->sustain_fire ? 0.92 : 1.0) : 0.0;

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
        v->gun_click_env = (p->click_amp > 0.0) ? (p->sustain_fire ? 0.92 : 1.0) : 0.0;
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
        // Friedlander pulse params. t+ derived from env_rate so existing
        // preset tunings still feel right: shorter env_rate → wider pulse
        // (grenade ~7ms, pistol ~1ms). Friedlander A = 1.5 is a good
        // default for the positive-phase decay shape.
        v->gun_phys_t = 0.0;
        v->gun_phys_t_plus = (3.0 / (p->env_rate > 100 ? p->env_rate : 100.0)) * sr;
        if (v->gun_phys_t_plus < 32.0) v->gun_phys_t_plus = 32.0;     // ≥ ~0.17ms
        if (v->gun_phys_t_plus > 4096.0) v->gun_phys_t_plus = 4096.0; // ≤ ~21ms
        v->gun_phys_friedlander_a = 1.5;
        v->gun_phys_neg_amp = 0.18;
        // Ground reflection — fixed ~3.5ms tap with 18% gain. Caps at
        // 1023 samples = ~5.3ms at 192kHz. Tunable per-preset later.
        v->gun_phys_echo_delay = 0.0035 * sr;
        if (v->gun_phys_echo_delay > 1023.0) v->gun_phys_echo_delay = 1023.0;
        v->gun_phys_echo_amp = 0.22;
        memset(v->gun_phys_echo_buf, 0, sizeof(v->gun_phys_echo_buf));
        v->gun_phys_echo_w = 0;
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
        v->gun_phys_t = 0.0;
        v->gun_phys_t_plus = 0.0;
        v->gun_phys_friedlander_a = 0.0;
        v->gun_phys_neg_amp = 0.0;
        v->gun_phys_echo_delay = 0.0;
        v->gun_phys_echo_amp = 0.0;
        v->gun_phys_echo_w = 0;
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

// Physical-model gunshot — Friedlander blast wave excitation feeding a
// DWG bore + parallel body modes + muzzle radiation HPF + ground-echo
// tap. Better for cavity-dominated weapons (grenade, RPG launch tube)
// where the bore length is meaningful.
//
// The Friedlander waveform models the actual pressure-vs-time curve of
// a free-air blast wave:
//   P(t) = P_peak · (1 − t/t+) · exp(−A·t/t+)        for 0 ≤ t ≤ t+
//   P(t) ≈ −P_peak · neg_amp · (1−tn) · exp(−2·tn)   for t > t+
//             (where tn = (t−t+) / (4·t+))
// (Friedlander 1946; widely used in blast-wave acoustics — see Mengual
//  et al. 2017 "Procedural Synthesis of Gunshot Sounds…")
//
// The ground-echo tap (a single delayed copy of the radiated signal,
// ~3-5 ms behind, attenuated) gives the spatial sense of an outdoor
// shot — without it, the whole thing sounds anechoic and wrong.
static inline double generate_gun_physical_sample(ACVoice *v, double sr) {
    // === Excitation: Friedlander envelope shaping a noise burst ===
    // Pure Friedlander pulses are too smooth between samples — the muzzle
    // radiation HPF (1-zero differentiator at α≈0.985) annihilates anything
    // that doesn't change between adjacent samples, killing the radiated
    // path entirely. So we use Friedlander as the AMPLITUDE ENVELOPE of a
    // noise burst (rather than the signal itself). The smooth shape gives
    // us the right onset/decay character; the noise content gives the HPF
    // and bore loop high-frequency material to actually radiate and ring.
    double t = v->gun_phys_t;
    double t_plus = v->gun_phys_t_plus;
    double A = v->gun_phys_friedlander_a;
    double pulse = 0.0;
    if (t < t_plus) {
        // Positive phase — sharp peak then exp decay.
        double f = t / t_plus;
        pulse = (1.0 - f) * exp(-A * f);
    } else if (t < t_plus * 5.0) {
        // Negative phase — sub-atmospheric dip after the wave passes.
        double tn = (t - t_plus) / (t_plus * 4.0);
        pulse = -v->gun_phys_neg_amp * (1.0 - tn) * exp(-2.0 * tn);
    }
    uint32_t n = xorshift32(&v->noise_seed);
    double white = ((double)n / (double)UINT32_MAX) * 2.0 - 1.0;
    // Smooth deterministic component + noise rider. noise_gain mixes the
    // turbulent content. The smooth term keeps low-freq energy for the
    // bore loop; the noise term feeds the radiation HPF + body modes.
    double excite = v->gun_pressure * pulse * (1.0 + v->gun_noise_gain * white);
    v->gun_phys_t += 1.0;

    // Secondary trigger — rifle N-wave or RPG delayed explosion. Restarts
    // the Friedlander pulse from t=0 with a scaled peak.
    if (v->gun_secondary_trig > 0.0) {
        v->gun_secondary_trig -= 1.0;
        if (v->gun_secondary_trig <= 0.0) {
            v->gun_phys_t = 0.0;
            v->gun_pressure *= v->gun_secondary_amp;
            v->gun_secondary_trig = 0.0;
        }
    }

    // Sustain fire — restart pulse at jitter scale.
    if (v->gun_sustain_fire && v->state == VOICE_ACTIVE
        && isinf(v->duration) && v->gun_retrig_period > 0.0) {
        v->gun_retrig_timer += 1.0 / sr;
        if (v->gun_retrig_timer >= v->gun_retrig_period) {
            v->gun_retrig_timer -= v->gun_retrig_period;
            v->gun_phys_t = 0.0;
            double j = (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
            // Tiny per-shot pressure variance around 1.0 (no permanent drift).
            v->gun_pressure *= 0.92 + j * 0.16;
        }
    }

    // Pitch sweep approach (ricochet — currently classic-only, kept for parity).
    if (v->gun_pitch_mult != v->gun_pitch_target) {
        v->gun_pitch_mult += (v->gun_pitch_target - v->gun_pitch_mult) * 0.00012;
    }
    double bore_delay = v->gun_bore_delay * v->gun_pitch_mult;
    if (bore_delay < 4.0) bore_delay = 4.0;
    if (bore_delay > 2040.0) bore_delay = 2040.0;

    // === Bore: closed breech (+refl) / open muzzle (−refl + LPF damping) ===
    const int BORE_N = 2048;
    double bore_out = whistle_frac_read(v->whistle_bore_buf, BORE_N,
                                        v->whistle_bore_w, bore_delay);
    v->gun_bore_lp = v->gun_bore_loss * (-bore_out)
                     + (1.0 - v->gun_bore_loss) * v->gun_bore_lp;
    double refl = v->gun_bore_lp;
    double into_bore = excite + refl * v->gun_breech_reflect;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)into_bore;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % BORE_N;

    // === Muzzle radiation: 1-zero HPF (open end emphasizes highs) ===
    double radiated = into_bore - v->gun_radiation_a * v->gun_rad_prev;
    v->gun_rad_prev = into_bore;

    // === Body modes: parallel pole-pair resonators on the excitation ===
    double body = 0.0;
    for (int i = 0; i < 3; i++) {
        double y = excite
                   + v->gun_body_a1[i] * v->gun_body_y1[i]
                   - v->gun_body_a2[i] * v->gun_body_y2[i];
        v->gun_body_y2[i] = v->gun_body_y1[i];
        v->gun_body_y1[i] = y;
        body += y * v->gun_body_amp[i];
    }

    double dry = radiated * 0.55 + body * 0.45;

    // === Ground reflection echo — short delayed copy. Even at low
    // amplitude this turns the dry shot into a "fired outdoors" shot.
    double echo_out = 0.0;
    if (v->gun_phys_echo_amp > 0.0 && v->gun_phys_echo_delay > 1.0) {
        const int ECHO_N = 1024;
        int read_pos = v->gun_phys_echo_w - (int)v->gun_phys_echo_delay;
        while (read_pos < 0) read_pos += ECHO_N;
        echo_out = (double)v->gun_phys_echo_buf[read_pos % ECHO_N]
                   * v->gun_phys_echo_amp;
        v->gun_phys_echo_buf[v->gun_phys_echo_w] = (float)dry;
        v->gun_phys_echo_w = (v->gun_phys_echo_w + 1) % ECHO_N;
    }

    return (dry + echo_out) * compute_envelope(v);
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
    case WAVE_HARP:
        s = generate_harp_sample(v, sample_rate);
        break;
    case WAVE_PIANO:
        s = generate_piano_sample(v, sample_rate);
        break;
    case WAVE_GMPIANO:
        s = generate_gmpiano_sample(v, sample_rate);
        break;
    case WAVE_EPIANO:
        s = generate_epiano_sample(v, sample_rate);
        break;
    case WAVE_PLUCK:
        s = generate_pluck_sample(v, sample_rate);
        break;
    default:
        s = 0.0;
    }

    // Smooth frequency toward target (uses precomputed alpha from caller)
    if (v->target_frequency > 0 && v->frequency != v->target_frequency) {
        v->frequency += (v->target_frequency - v->frequency) * 0.0003; // ~5ms at 192kHz
    }

    // Advance phase for basic oscillators; whistle/gun/harp/piano and the GM
    // synthesis voices (gmpiano/epiano/pluck) manage their own phase state.
    if (v->type != WAVE_WHISTLE && v->type != WAVE_GUN && v->type != WAVE_HARP &&
        v->type != WAVE_PIANO && v->type != WAVE_GMPIANO && v->type != WAVE_EPIANO &&
        v->type != WAVE_PLUCK) {
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
    /* tanh-based soft limiter — much smoother than the previous
     * piecewise clipper which had a kinked transfer curve above
     * the knee, producing "fuzzy" / tinny distortion at high gain.
     * tanh(x * 0.9) stays linear to ~0.6, eases through 0.85, and
     * asymptotes at ±1.0. Adds only small even-order harmonics
     * which sound "warm" instead of harsh. */
    return tanh(x * 0.9) / tanh(0.9);
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
    int32_t *buffer32 = NULL;
    if (audio->use_s32)
        buffer32 = calloc(period_frames * AUDIO_CHANNELS, sizeof(int32_t));
    if (!buffer || (audio->use_s32 && !buffer32)) {
        fprintf(stderr, "[audio] thread: alloc failed\n"); return NULL;
    }
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

    /* Pin the audio thread to the last online CPU. CPU 0 typically
     * services timer/network/USB IRQs; isolating audio on a separate
     * core tightens the jitter ceiling without affecting median.
     * Disable with AC_AUDIO_NO_PIN=1 if it conflicts with isolcpus. */
    if (!getenv("AC_AUDIO_NO_PIN")) {
        long ncpu = sysconf(_SC_NPROCESSORS_ONLN);
        if (ncpu > 1) {
            int target = (int)(ncpu - 1);
            cpu_set_t cs;
            CPU_ZERO(&cs);
            CPU_SET(target, &cs);
            if (pthread_setaffinity_np(pthread_self(), sizeof(cs), &cs) != 0)
                fprintf(stderr, "[audio] Warning: couldn't pin audio thread to CPU %d\n", target);
            else
                fprintf(stderr, "[audio] Pinned to CPU %d (of %ld online)\n", target, ncpu);
        }
    }

    /* Optional jitter benchmark: AC_LATENCY_BENCH=1 makes the audio
     * thread record per-period wall-clock intervals and emit a
     * single-line stats summary every PERIODS_PER_REPORT iterations.
     * Output format (one line per report, parseable by latency.mjs):
     *   [ac-latency] period_us=<expected> n=<count> min=<us> p50=<us>
     *     mean=<us> p99=<us> max=<us> over_period_us=<count> xruns=<count>
     */
    const int latency_bench = getenv("AC_LATENCY_BENCH") &&
                              getenv("AC_LATENCY_BENCH")[0] == '1';
    const int PERIODS_PER_REPORT = 1024;
    long expected_period_us = (long)(((double)period_frames / (double)rate) * 1e6);
    /* Keep a small ring of recent intervals so we can compute p50/p99
     * without storing every sample for the lifetime of the program. */
    long *bench_us = NULL;
    int bench_count = 0;
    long bench_min = LONG_MAX, bench_max = 0, bench_sum = 0;
    long bench_over_period = 0;  // periods that took > 1.5x expected
    struct timespec bench_prev_ts = {0};
    if (latency_bench) {
        bench_us = (long *)calloc(PERIODS_PER_REPORT, sizeof(long));
        /* bench_prev_ts left at {0,0} — set on first iteration below
         * so the first delta is skipped (it would include startup
         * prefill, not a real period interval). */
        fprintf(stderr, "[ac-latency] benchmark enabled — period_us=%ld report=%d periods\n",
                expected_period_us, PERIODS_PER_REPORT);
    }

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

            // Mix the named one-shot bank voices (zoo / lasers). Each voice
            // points at its own buffer in oneshot_bank[]; up to
            // ONESHOT_MAX_VOICES concurrent.
            mix_oneshot_voices(rate, &mix_l, &mix_r);

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

            // Smooth master volume + drive toward target (same 1s time const)
            if (audio->master_volume != audio->target_master_volume) {
                audio->master_volume += (audio->target_master_volume - audio->master_volume) * 0.00005f;
            }
            if (audio->drive_mix != audio->target_drive_mix) {
                audio->drive_mix += (audio->target_drive_mix - audio->drive_mix) * 0.00005f;
            }
            if (audio->wobble_mix != audio->target_wobble_mix) {
                audio->wobble_mix += (audio->target_wobble_mix - audio->wobble_mix) * 0.00005f;
            }

            // Save dry signal before FX chain
            double dry_l = mix_l, dry_r = mix_r;

            // Capture recent dry output for true reverse replay. This stores
            // the actual mixed audio (not note events) before room/glitch/TTS
            // so the reverse replay can run back through the live FX chain.
            //
            // When `output_history_paused` is set (by notepat while spacebar
            // is held), we skip this write entirely — the reverse-playback
            // voice being fed back through the speaker mix would otherwise
            // re-enter the ring and double-layer on the original audio. The
            // pause ONLY affects capture; the ring contents and read_pos
            // are untouched so replay continues from the existing snapshot.
            if (audio->output_history_buf && audio->output_history_size > 0
                && !audio->output_history_paused) {
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

            // Wobble / flange — modulated short-delay blend. Writes every
            // sample into the ring regardless of wet mix (so the ring
            // stays warm for instant-on when the slider turns up), then
            // reads a sample `delay_samples` behind the write head with
            // the delay itself sweeping via a slow LFO. Tiny feedback
            // (0.45) thickens the tail so moderate mix settings already
            // produce the characteristic jet-sweep coloration.
            if (audio->wobble_buf_l && audio->wobble_buf_size > 0) {
                int size = audio->wobble_buf_size;
                int mask = size - 1; // size is power of two (1024)
                int wp = audio->wobble_write_pos;
                float wmix = audio->wobble_mix;
                // Sweep: 2-10 ms at 48 kHz = 96..480 samples.
                float lfo = (float)sin((double)audio->wobble_lfo_phase);
                float delay_samples = 96.0f + (lfo * 0.5f + 0.5f) * 384.0f;
                audio->wobble_lfo_phase += audio->wobble_lfo_rate;
                if (audio->wobble_lfo_phase > 6.283185307179586f)
                    audio->wobble_lfo_phase -= 6.283185307179586f;
                // Fractional read with linear interp.
                float rp = (float)wp - delay_samples;
                while (rp < 0) rp += size;
                int rp_i = (int)rp;
                float rp_f = rp - (float)rp_i;
                float dly_l = audio->wobble_buf_l[rp_i & mask] * (1.0f - rp_f)
                            + audio->wobble_buf_l[(rp_i + 1) & mask] * rp_f;
                float dly_r = audio->wobble_buf_r[rp_i & mask] * (1.0f - rp_f)
                            + audio->wobble_buf_r[(rp_i + 1) & mask] * rp_f;
                // Feedback into the ring (for sustained "zing").
                audio->wobble_buf_l[wp & mask] = (float)mix_l + dly_l * 0.45f;
                audio->wobble_buf_r[wp & mask] = (float)mix_r + dly_r * 0.45f;
                audio->wobble_write_pos = (wp + 1) & mask;
                if (wmix > 0.001f) {
                    mix_l = mix_l * (1.0 - wmix) + (double)dly_l * wmix;
                    mix_r = mix_r * (1.0 - wmix) + (double)dly_r * wmix;
                }
            }

            // User-controlled drive (tanh soft-saturation) BEFORE system
            // volume so the harmonic character is independent of hardware
            // gain. drive_mix is a dry/wet blend: 0 = pure bypass, 1 = fully
            // driven (pre-gain × 6 into tanh, attenuated back to roughly
            // unity peak). At mid settings you get pleasing tube-ish warmth.
            if (audio->drive_mix > 0.001f) {
                float dm = audio->drive_mix;
                float pre_gain = 1.0f + dm * 5.0f;
                double driven_l = tanh(mix_l * pre_gain) * 0.8;
                double driven_r = tanh(mix_r * pre_gain) * 0.8;
                mix_l = mix_l * (1.0 - dm) + driven_l * dm;
                mix_r = mix_r * (1.0 - dm) + driven_r * dm;
            }

            // User-controlled master volume (0..2 = 0..200%). Applied after
            // drive so the slider feels like a "louder/quieter" control that
            // doesn't change the tone character the user dialled in.
            {
                float mv = audio->master_volume;
                mix_l *= mv;
                mix_r *= mv;
            }

            // Apply system volume (software gain). system_volume can go
            // above 100 on SOF cards where the DSP pipeline has -6dB+
            // headroom and the amp needs boosting to reach normal levels.
            // -1 means no Master mixer found — treat as 100% baseline.
            {
                int sv = audio->system_volume;
                if (sv < 0) sv = 100;
                double vol = sv * 0.01; // 0..4 (up to 400% → 16× linear)
                vol = vol * vol;         // squared curve
                mix_l *= vol;
                mix_r *= vol;
            }

            // Soft clip and convert to int16
            mix_l = soft_clip(mix_l);
            mix_r = soft_clip(mix_r);

            /* 32000 = ~97% of int16 max. Was 26000 (~79%), wasting
             * ~2dB of peak output. Combined with higher soft_clip
             * knee this gives ~4dB more audible loudness before any
             * compression artifacts. */
            buffer[i * 2] = (int16_t)(mix_l * 32000);
            buffer[i * 2 + 1] = (int16_t)(mix_r * 32000);

            /* DAPM keepalive: inject low-frequency dither (alternating
             * every 8 samples = 3kHz, well below 24kHz Nyquist edge).
             * ±8 int16 → ±2048 int32 ≈ -72 dBFS, inaudible through
             * DAC anti-alias filter. Previous ±32 @ 24kHz alternation
             * leaked through as faint hiss at high gain. */
            if (buffer[i * 2] == 0 && buffer[i * 2 + 1] == 0) {
                int16_t d = ((i >> 3) & 1) ? 8 : -8;
                buffer[i * 2]     = d;
                buffer[i * 2 + 1] = -d;
            }

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
        /* Tee to parallel PCM (sof-rt5682+max98360a auto-route).
         * Best-effort, never blocks the primary write — short writes,
         * EPIPE underruns, and even outright failures are tolerated
         * because the *real* output is the primary PCM. The DAPM
         * jack-sense in the codec mutes whichever side isn't being
         * driven by the active jack state. */
        snd_pcm_t *pcm2 = (snd_pcm_t *)audio->headphone_pcm;
        if (pcm2) {
            int rem2 = (int)period_frames;
            int off2 = 0;
            while (rem2 > 0) {
                int f2 = snd_pcm_writei(pcm2,
                                         buffer + off2 * AUDIO_CHANNELS,
                                         rem2);
                if (f2 == -EAGAIN) break; /* don't spin on secondary */
                if (f2 < 0) {
                    snd_pcm_recover(pcm2, f2, 1);
                    break;
                }
                rem2 -= f2; off2 += f2;
            }
        }
        /* Widen int16→int32 for S32_LE PCMs (SOF topology).
         * The SSP1 BE DAI runs S24_LE. SOF DSP uses the bottom 24
         * bits of the S32 container (bits 23:0). Shifting int16 by
         * 8 places our 16-bit audio in bits 23:8, which fills the
         * top portion of the 24-bit window — correct for S24-in-S32
         * bottom-aligned format. (<<16 put data in bits 31:16 which
         * the DSP's 24-bit window barely saw → super quiet.) */
        const void *write_buf = buffer;
        if (buffer32) {
            for (int j = 0; j < (int)(period_frames * AUDIO_CHANNELS); j++)
                buffer32[j] = (int32_t)buffer[j] << 8;
            write_buf = buffer32;
        }
        int remaining = (int)period_frames;
        int offset = 0;
        while (remaining > 0) {
            const void *wptr = buffer32
                ? (const void *)(buffer32 + offset * AUDIO_CHANNELS)
                : (const void *)(buffer + offset * AUDIO_CHANNELS);
            /* mmap_writei skips a buffer copy versus writei; falls
             * through to writei when access wasn't negotiated as
             * MMAP_INTERLEAVED. */
            int frames = audio->use_mmap
                ? snd_pcm_mmap_writei(pcm, wptr, remaining)
                : snd_pcm_writei(pcm, wptr, remaining);
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

        /* Per-period jitter measurement (AC_LATENCY_BENCH=1).
         * Time between successive writei completions = actual delivered
         * period. Compare to expected to expose audio-thread scheduling
         * jitter — the empirical floor for audio-side latency. */
        if (latency_bench && bench_us) {
            struct timespec now_ts;
            clock_gettime(CLOCK_MONOTONIC, &now_ts);
            if (bench_prev_ts.tv_sec == 0 && bench_prev_ts.tv_nsec == 0) {
                /* First iteration — establish the time origin and skip;
                 * the first delta would include startup prefill. */
                bench_prev_ts = now_ts;
            } else {
                long delta_us = (now_ts.tv_sec - bench_prev_ts.tv_sec) * 1000000L +
                                (now_ts.tv_nsec - bench_prev_ts.tv_nsec) / 1000L;
                bench_prev_ts = now_ts;
                bench_us[bench_count] = delta_us;
                if (delta_us < bench_min) bench_min = delta_us;
                if (delta_us > bench_max) bench_max = delta_us;
                bench_sum += delta_us;
                if (delta_us > expected_period_us * 3 / 2) bench_over_period++;
                bench_count++;
            }
            if (bench_count >= PERIODS_PER_REPORT) {
                /* qsort to compute p50/p99. n=1024 → ~10k comparisons,
                 * runs in tens of µs once per second of audio — well
                 * under one period budget on the RT audio thread. */
                int n = bench_count;
                qsort(bench_us, n, sizeof(long), bench_cmp_long);
                long p50 = bench_us[n / 2];
                long p99 = bench_us[(int)(n * 0.99)];
                long mean = bench_sum / n;
                fprintf(stderr,
                        "[ac-latency] period_us=%ld n=%d min=%ld p50=%ld mean=%ld p99=%ld max=%ld over_period=%ld xruns=%lu\n",
                        expected_period_us, n, bench_min, p50, mean,
                        p99, bench_max, bench_over_period, xrun_count);
                bench_count = 0;
                bench_min = LONG_MAX; bench_max = 0; bench_sum = 0;
                bench_over_period = 0;
            }
        }
    }

    free(bench_us);
    free(buffer);
    free(buffer32);
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

    // Build the sine wavetable used by the GM modal/FM voices (idempotent).
    wt_sin_init();

    // Load piano sample bank from /samples/piano/. Idempotent: safe to
    // call from both audio_init paths in ac-native.c. Bank is global
    // (not on the audio struct) since it's read-only once loaded.
    static int piano_bank_loaded = 0;
    if (!piano_bank_loaded) {
        load_piano_bank();
        piano_bank_loaded = 1;
    }

    // Load the named one-shot bank (zoo + lasers + future kits). Global,
    // process-lifetime, idempotent — same pattern as the piano bank.
    static int oneshot_bank_loaded = 0;
    if (!oneshot_bank_loaded) {
        load_oneshot_bank();
        oneshot_bank_loaded = 1;
    }
    // Voice slots are static, just make sure no stale state leaks in.
    for (int i = 0; i < ONESHOT_MAX_VOICES; i++) {
        oneshot_voices[i].active = 0;
    }

    // Allocate reverb buffers
    audio->room_size = ROOM_SIZE;
    audio->room_mix = 0.0f;  // Start dry, trackpad Y controls
    audio->room_enabled = 1; // Always on, mix controls wet amount
    audio->glitch_mix = 0.0f;
    audio->target_glitch_mix = 0.0f;
    audio->fx_mix = 1.0f;    // FX chain fully wet by default
    audio->target_fx_mix = 1.0f;
    // User master volume starts at 1.0 (unity gain) — the pre-existing
    // system_volume path still provides the hardware mixer control, so
    // this is a per-user soft gain on top.
    audio->master_volume = 1.0f;
    audio->target_master_volume = 1.0f;
    audio->drive_mix = 0.0f;  // Clean bypass until user dials drive
    audio->target_drive_mix = 0.0f;
    // Wobble / flange — 1024-sample ring covers up to ~21 ms @ 48 kHz
    // which is well past the flanger sweet spot (1-10 ms). Power-of-two
    // size lets the read index use `& (size-1)` instead of `%`.
    audio->wobble_buf_size = 1024;
    audio->wobble_buf_l = calloc(audio->wobble_buf_size, sizeof(float));
    audio->wobble_buf_r = calloc(audio->wobble_buf_size, sizeof(float));
    audio->wobble_write_pos = 0;
    audio->wobble_lfo_phase = 0.0f;
    // LFO rate 0.4 Hz — slow sweep, reads as "wobble" not "chorus"
    audio->wobble_lfo_rate = 2.0f * 3.14159265358979f * 0.4f / 48000.0f;
    audio->wobble_mix = 0.0f;
    audio->target_wobble_mix = 0.0f;
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

    // SOF rt5682+max98360a (G7/Drawcia and friends) splits playback across two
    // PCMs: SSP0 → RT5682 headphones (PCM 0) and SSP1 → MAX98360A speakers
    // (PCM 1). The historical hw:0,0 default routes everything to headphones,
    // which is why speakers stayed silent even with the Speaker UCM verb fully
    // applied. Ask UCM what PCM it maps Speaker(s) to and try that FIRST so
    // playback hits the speaker amp by default; the headphone PCM still wins
    // if/when AC_AUDIO_DEVICE overrides or jack-sense flips routing later.
    char ucm_speaker_pcm[64] = "";
    char ucm_headphone_pcm[64] = "";
    {
        char card_id[32] = "";
        int spk_card = 0; /* card index where Speaker UCM lives */
        for (int c = 0; c < 4 && !card_id[0]; c++) {
            char p[64]; snprintf(p, sizeof(p), "/proc/asound/card%d/id", c);
            FILE *fp = fopen(p, "r");
            if (fp) {
                if (fgets(card_id, sizeof(card_id), fp)) {
                    char *nl = strchr(card_id, '\n'); if (nl) *nl = 0;
                }
                fclose(fp);
                if (card_id[0]) spk_card = c;
            }
        }
        if (card_id[0]) {
            const char *cands[] = { card_id, "sof-rt5682", "sof-cs42l42",
                                    "sof-nau8825", "sof-da7219", NULL };
            snd_use_case_mgr_t *uc = NULL;
            for (int i = 0; cands[i] && (!ucm_speaker_pcm[0] || !ucm_headphone_pcm[0]); i++) {
                if (snd_use_case_mgr_open(&uc, cands[i]) != 0) continue;
                if (snd_use_case_set(uc, "_verb", "HiFi") == 0) {
                    const char *spk_names[] = { "Speaker", "Speakers", NULL };
                    for (int s = 0; spk_names[s] && !ucm_speaker_pcm[0]; s++) {
                        char id[64]; const char *val = NULL;
                        snprintf(id, sizeof(id), "PlaybackPCM/%s", spk_names[s]);
                        if (snd_use_case_get(uc, id, &val) == 0 && val) {
                            /* UCM v2 returns strings like
                             * "_ucm0002.hw:sofrt5682,0" — that is a
                             * UCM-internal namespace tag, not a path
                             * snd_pcm_open accepts. Strip the
                             * "_ucmNNNN." prefix to get the underlying
                             * "hw:CARD,DEV" form, then convert the card
                             * id to a numeric index since snd_pcm_open
                             * also rejects "hw:sofrt5682,0". */
                            const char *clean = val;
                            const char *dot = strchr(clean, '.');
                            if (dot && strncmp(clean, "_ucm", 4) == 0)
                                clean = dot + 1;
                            const char *comma = strrchr(clean, ',');
                            if (comma && strncmp(clean, "hw:", 3) == 0) {
                                snprintf(ucm_speaker_pcm,
                                         sizeof(ucm_speaker_pcm),
                                         "hw:%d%s", spk_card, comma);
                            } else {
                                snprintf(ucm_speaker_pcm,
                                         sizeof(ucm_speaker_pcm),
                                         "%s", clean);
                            }
                            fprintf(stderr,
                                    "[audio] UCM Speaker PCM: raw=%s -> %s (%s/%s)\n",
                                    val, ucm_speaker_pcm, cands[i], spk_names[s]);
                            free((void *)val);
                        }
                    }
                    const char *hp_names[] = { "Headphone", "Headphones",
                                                "Headset", NULL };
                    for (int s = 0; hp_names[s] && !ucm_headphone_pcm[0]; s++) {
                        char id[64]; const char *val = NULL;
                        snprintf(id, sizeof(id), "PlaybackPCM/%s", hp_names[s]);
                        if (snd_use_case_get(uc, id, &val) == 0 && val) {
                            const char *clean = val;
                            const char *dot = strchr(clean, '.');
                            if (dot && strncmp(clean, "_ucm", 4) == 0)
                                clean = dot + 1;
                            const char *comma = strrchr(clean, ',');
                            if (comma && strncmp(clean, "hw:", 3) == 0) {
                                snprintf(ucm_headphone_pcm,
                                         sizeof(ucm_headphone_pcm),
                                         "hw:%d%s", spk_card, comma);
                            } else {
                                snprintf(ucm_headphone_pcm,
                                         sizeof(ucm_headphone_pcm),
                                         "%s", clean);
                            }
                            fprintf(stderr,
                                    "[audio] UCM Headphone PCM: raw=%s -> %s (%s/%s)\n",
                                    val, ucm_headphone_pcm, cands[i], hp_names[s]);
                            free((void *)val);
                        }
                    }
                }
                snd_use_case_mgr_close(uc);
                uc = NULL;
            }
        }
    }

    /* Build the device probe list. If UCM gave us a Speaker PCM, prepend it
     * (and a `plug:` wrapped variant for rate negotiation safety). The legacy
     * fallback list still runs after, so non-SOF boards behave as before. */
    const char *devices_default[] = {
        "hw:0,0", "hw:1,0", "hw:0,1", "hw:1,1",
        "hw:0,2", "hw:0,3", "hw:1,2", "hw:1,3",
        "plughw:0,0", "plughw:1,0",
        "default", NULL
    };
    const char *devices_with_spk[16] = {0};
    const char **devices = devices_default;
    char ucm_speaker_plug[80] = "";
    if (ucm_speaker_pcm[0]) {
        snprintf(ucm_speaker_plug, sizeof(ucm_speaker_plug),
                 "plughw%s", ucm_speaker_pcm + 2); /* hw:0,0 → plughw:0,0 */
        int n = 0;
        /* Prefer raw hw: first — SOF topology FE PCM runs at S32_LE
         * internally, and we now negotiate S32_LE directly so the DSP
         * does zero conversion. plughw: as fallback if hw: fails. */
        devices_with_spk[n++] = ucm_speaker_pcm;
        devices_with_spk[n++] = ucm_speaker_plug;
        for (int i = 0; devices_default[i] && n < 15; i++)
            devices_with_spk[n++] = devices_default[i];
        devices_with_spk[n] = NULL;
        devices = devices_with_spk;
    }
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

    // SOF detection must happen FIRST, because the format choice depends on it.
    // The S32_LE write path uses `<< 8` shift (writes int16 into the low 24
    // bits of int32), which is correct for SOF's MAX98360A 24-bit DSP but
    // 256× too quiet for HDA's true 32-bit DAC — even though both codecs
    // accept S32_LE in their hw_params. Force S16_LE on non-SOF hardware.
    int sof_active = (access("/sys/module/snd_sof/initstate", F_OK) == 0) ||
                     (access("/sys/module/snd_sof_pci/initstate", F_OK) == 0);

    // Configure ALSA — negotiate rate dynamically.
    // Try preferred rates from highest to lowest. The hardware decides what it
    // actually supports; we adapt period/buffer sizes to match the negotiated rate.
    snd_pcm_hw_params_t *params;
    snd_pcm_hw_params_alloca(&params);
    snd_pcm_hw_params_any(pcm, params);
    /* Prefer MMAP_INTERLEAVED: snd_pcm_mmap_writei skips the kernel
     * ring-buffer copy that snd_pcm_writei does, saving a fraction
     * of a millisecond on HDA paths. Fall back to RW_INTERLEAVED if
     * the hardware/driver doesn't expose mmap (rare on real PCMs;
     * common on plughw with rate conversion). */
    audio->use_mmap = 0;
    if (snd_pcm_hw_params_set_access(pcm, params,
                                      SND_PCM_ACCESS_MMAP_INTERLEAVED) == 0) {
        audio->use_mmap = 1;
        fprintf(stderr, "[audio] Negotiated MMAP_INTERLEAVED access\n");
    } else {
        snd_pcm_hw_params_any(pcm, params);
        snd_pcm_hw_params_set_access(pcm, params, SND_PCM_ACCESS_RW_INTERLEAVED);
        fprintf(stderr, "[audio] Negotiated RW_INTERLEAVED access (no mmap)\n");
    }
    /* SOF topology FE PCMs use S32_LE internally; the SSP1 BE DAI
     * (MAX98360A) runs S24_LE. Writing S16_LE to this pipeline
     * causes 48dB attenuation + quantization noise ("crunchy quiet").
     * Try S32_LE on SOF only. On HDA / USB / generic codecs, S16_LE
     * is the universally-correct choice — even when the codec
     * advertises S32_LE support, our int16<<8 conversion drops 8 of
     * the high bits and produces inaudibly-quiet output. */
    audio->use_s32 = 0;
    if (sof_active &&
        snd_pcm_hw_params_set_format(pcm, params, SND_PCM_FORMAT_S32_LE) == 0) {
        audio->use_s32 = 1;
        fprintf(stderr, "[audio] Negotiated S32_LE format (SOF)\n");
    } else {
        /* Re-negotiate from scratch with S16_LE; preserve the access
         * mode we picked above. */
        snd_pcm_hw_params_any(pcm, params);
        snd_pcm_hw_params_set_access(pcm, params,
                                     audio->use_mmap
                                         ? SND_PCM_ACCESS_MMAP_INTERLEAVED
                                         : SND_PCM_ACCESS_RW_INTERLEAVED);
        snd_pcm_hw_params_set_format(pcm, params, SND_PCM_FORMAT_S16_LE);
        fprintf(stderr, "[audio] Negotiated S16_LE format%s\n",
                sof_active ? " (S32_LE rejected)" : " (non-SOF, forced)");
    }
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

    // Period + buffer sizing. The old config aimed for ~1ms latency (period =
    // rate/1000, buffer = 4 periods) which works on HDA-direct codecs but
    // breaks SOF+MAX98360A on Jasper Lake Chromebooks: the MAX98357A DAPM
    // event handler toggles the amp's SD_MODE GPIO on every PMU/PMD event,
    // and with a 4ms buffer the stream underruns constantly → DAPM rapid-
    // cycles the amp on/off → audio never stabilizes → speakers stay silent
    // despite mixer, codec, and GPIO all looking correct. We saw 10,686
    // sdmode toggles in a single boot's kmsg on the G7 at the 1ms setting.
    //
    // Period + buffer: 20ms/80ms on SOF (avoids MAX98357A SD_MODE GPIO
    // thrash), 1ms/4ms on HDA-direct paths (tight latency safe there).
    // sof_active was set above before format negotiation.
    snd_pcm_uframes_t period;
    snd_pcm_uframes_t buffer_size;
    if (sof_active) {
        period = rate / 50;        // 20ms (960 frames at 48kHz)
        buffer_size = period * 4;  // 80ms total — larger buffer
                                   // reduces XRUN-induced "fuzzy"
                                   // audio at high software gain.
                                   // Previous 10ms/40ms produced
                                   // 96/480-frame short writes.
        fprintf(stderr, "[audio] SOF platform detected — period=%lu buffer=%lu (20ms/80ms)\n",
                (unsigned long)period, (unsigned long)buffer_size);
    } else {
        period = rate / 1000;      // 1ms on HDA-direct paths
        if (period < 64) period = 64;
        buffer_size = period * 4;
    }
    snd_pcm_hw_params_set_period_size_near(pcm, params, &period, 0);
    snd_pcm_hw_params_set_buffer_size_near(pcm, params, &buffer_size);

    err = snd_pcm_hw_params(pcm, params);
    if (err < 0) {
        fprintf(stderr, "[audio] Cannot configure ALSA at %uHz: %s\n", rate, snd_strerror(err));
        // Last resort: try plughw with default params
        fprintf(stderr, "[audio] Trying plughw fallback...\n");
        snd_pcm_close(pcm);
        err = snd_pcm_open(&pcm, "plughw:0,0", SND_PCM_STREAM_PLAYBACK, 0);
        if (err >= 0) {
            audio->use_mmap = 0;  // plughw rate-conv path: no mmap
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

    /* Open the *other* PCM for jack-sense auto-routing.
     *
     * On sof-rt5682+max98360a, the SOF topology exposes two FE PCMs:
     *   PCM 0 (UCM "Headphone") → SSP0 → RT5682 codec → headphone jack
     *   PCM 1 (UCM "Speaker"  ) → SSP1 → MAX98360A   → speaker amp
     *
     * Each PCM goes to a *different* DAI, and the codec's DAPM jack-sense
     * mutes whichever side isn't currently in use. So if we open BOTH and
     * tee the same audio to both, the hardware automatically picks the
     * right output: speakers when no jack is plugged, headphones when one
     * is plugged. No userspace jack monitor needed.
     *
     * Only fires when the secondary PCM is a different device than the one
     * we already opened — duplicate-open of the same hw: device would just
     * fail with -EBUSY. */
    audio->headphone_pcm = NULL;
    {
        const char *secondary = NULL;
        /* Opt-in: opening the headphone PCM at boot was powering up
         * the HP DAPM path even with UCM Headphones disabled, which
         * overrode jack-sense and silenced the MAX98360A amp. Keep
         * the secondary PCM closed by default; a future jack-watcher
         * thread will open/close it in response to plug events. Set
         * AC_AUDIO_TEE=1 to force-open anyway (for ThinkPad etc.
         * single-PCM HDA hardware, where it's a noop). */
        const char *tee_env = getenv("AC_AUDIO_TEE");
        int tee_enabled = (tee_env && tee_env[0] == '1');
        if (tee_enabled && ucm_speaker_pcm[0] && ucm_headphone_pcm[0] &&
            strcmp(ucm_speaker_pcm, ucm_headphone_pcm) != 0) {
            /* Pick whichever the main PCM didn't open. */
            if (strstr(audio->audio_device, ucm_speaker_pcm))
                secondary = ucm_headphone_pcm;
            else if (strstr(audio->audio_device, ucm_headphone_pcm))
                secondary = ucm_speaker_pcm;
            else
                secondary = ucm_headphone_pcm; /* legacy fallback opened */
        }
        if (secondary) {
            snd_pcm_t *pcm2 = NULL;
            int e2 = snd_pcm_open(&pcm2, secondary,
                                   SND_PCM_STREAM_PLAYBACK, 0);
            if (e2 == 0) {
                /* Same params as the main PCM so audio thread can write
                 * the same int16 buffer to both without resampling. */
                snd_pcm_hw_params_t *hp; snd_pcm_hw_params_alloca(&hp);
                snd_pcm_hw_params_any(pcm2, hp);
                snd_pcm_hw_params_set_access(pcm2, hp,
                                             SND_PCM_ACCESS_RW_INTERLEAVED);
                snd_pcm_hw_params_set_format(pcm2, hp, SND_PCM_FORMAT_S16_LE);
                snd_pcm_hw_params_set_channels(pcm2, hp, AUDIO_CHANNELS);
                unsigned int r2 = audio->actual_rate;
                snd_pcm_hw_params_set_rate_near(pcm2, hp, &r2, 0);
                snd_pcm_uframes_t p2 = audio->actual_period;
                snd_pcm_hw_params_set_period_size_near(pcm2, hp, &p2, 0);
                snd_pcm_uframes_t b2 = audio->actual_period * 4;
                snd_pcm_hw_params_set_buffer_size_near(pcm2, hp, &b2);
                int herr = snd_pcm_hw_params(pcm2, hp);
                if (herr == 0) {
                    snd_pcm_prepare(pcm2);
                    audio->headphone_pcm = pcm2;
                    fprintf(stderr,
                            "[audio] Parallel PCM opened: %s (%uHz, %lufrm) — auto-routing enabled\n",
                            secondary, r2, (unsigned long)p2);
                } else {
                    fprintf(stderr,
                            "[audio] Parallel PCM hw_params failed for %s: %s — auto-routing disabled\n",
                            secondary, snd_strerror(herr));
                    snd_pcm_close(pcm2);
                }
            } else {
                fprintf(stderr,
                        "[audio] Parallel PCM open failed for %s: %s — auto-routing disabled\n",
                        secondary, snd_strerror(e2));
            }
        }
    }

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

    /* Log the actual negotiated params — channels and format are
     * particularly important for diagnosing the "crunchy quiet" bug
     * on SOF boards where SSP1 may expect different bit depth. */
    {
        snd_pcm_format_t fmt;
        unsigned int ch = 0;
        snd_pcm_hw_params_get_format(params, &fmt);
        snd_pcm_hw_params_get_channels(params, &ch);
        fprintf(stderr, "[audio] ALSA: %uHz %uch fmt=%s period=%lu buf=%lu (%.1fms)\n",
                rate, ch, snd_pcm_format_name(fmt),
                (unsigned long)period, (unsigned long)buffer_size,
                (double)period / rate * 1000.0);
    }
    snprintf(audio->audio_status, sizeof(audio->audio_status),
             "ok %uHz %lufrm", rate, (unsigned long)period);
    if (rate != AUDIO_SAMPLE_RATE)
        fprintf(stderr, "[audio] WARNING: got %uHz instead of %dHz\n", rate, AUDIO_SAMPLE_RATE);

    // ChromeOS UCM verb activation. sof-rt5682 on Jasper Lake has no
    // upstream UCM, so without this the Speaker verb's csets
    // ('Spk Switch on' plus DSP pipeline routes) never fire and the
    // MAX98360A amp receives no I2S even with SD_MODE asserted. The
    // WeirdTreeThing/alsa-ucm-conf-cros bundle in /usr/share/alsa/ucm2/
    // provides the downstream ChromeOS versions. Noop on boards whose
    // UCM is already upstream (ThinkPad HDA, Macs) — snd_use_case_mgr_open
    // returns -ENOENT and we fall through to the manual mixer path below.
    {
        char card_id[32] = "";
        char id_path[64];
        snprintf(id_path, sizeof(id_path), "/proc/asound/card%d/id", card_idx);
        FILE *idfp = fopen(id_path, "r");
        if (idfp) {
            if (fgets(card_id, sizeof(card_id), idfp)) {
                char *nl = strchr(card_id, '\n'); if (nl) *nl = 0;
            }
            fclose(idfp);
        }
        if (card_id[0]) {
            /* The kernel strips hyphens from card IDs (so our machine
             * driver name `jsl_rt5682_def` + topology `sof-rt5682` both
             * become card id `sofrt5682`). The ChromeOS UCM tree keeps
             * the canonical hyphenated names. Try a few permutations so
             * whichever matches wins. */
            const char *candidates[] = {
                card_id,       /* e.g. "sofrt5682" */
                "sof-rt5682",
                "sof-cs42l42",
                "sof-nau8825",
                "sof-da7219",
                NULL
            };
            snd_use_case_mgr_t *uc = NULL;
            int uerr = -1;
            const char *opened = NULL;
            for (int i = 0; candidates[i]; i++) {
                uerr = snd_use_case_mgr_open(&uc, candidates[i]);
                if (uerr == 0) { opened = candidates[i]; break; }
            }
            if (uerr == 0) {
                fprintf(stderr, "[audio] UCM: opened '%s' (card=%s)\n",
                        opened, card_id);
                if (snd_use_case_set(uc, "_verb", "HiFi") == 0) {
                    fprintf(stderr, "[audio] UCM: _verb=HiFi set\n");
                } else {
                    fprintf(stderr, "[audio] UCM: _verb=HiFi failed\n");
                }
                /* Enable ONLY Speaker at boot — enumerating every device
                 * (including Headphones/Headset) was running ChromeOS
                 * UCM EnableSequences that set `Headphone Jack Switch on`
                 * + `HPOL/HPOR Playback Switch 1`. That forces DAPM to
                 * route audio through the RT5682 headphone path and
                 * powers down the MAX98360A amp (kmsg showed `sdmode
                 * to 0` at 35s and never recovering).
                 *
                 * The rt5682-init BootSequence from WeirdTreeThing's
                 * UCM deliberately ships with HP jack/switch OFF so
                 * the speaker amp stays live when nothing is plugged
                 * in. Keep that intact — only run the Speaker (and
                 * mic) EnableSequence, not any headphone one. Jack-
                 * plug routing becomes a later follow-up (a jack-
                 * state watcher thread that flips _enadev on plug). */
                const char **devlist = NULL;
                int ndev = snd_use_case_get_list(uc, "_devices/HiFi",
                                                 &devlist);
                int enabled_speaker = 0;
                if (ndev > 0 && devlist) {
                    for (int i = 0; i < ndev; i += 2) {
                        const char *dev = devlist[i];
                        if (!dev || !dev[0]) continue;
                        /* Skip anything that would re-enable the
                         * headphone path at boot. Speakers first,
                         * mics/HDMI are safe (no DAPM routing to HP). */
                        if (strstr(dev, "Headphone") ||
                            strstr(dev, "Headset") ||
                            strstr(dev, "Headphones")) {
                            fprintf(stderr,
                                    "[audio] UCM: skip _enadev=%s (jack-gated)\n",
                                    dev);
                            continue;
                        }
                        int rr = snd_use_case_set(uc, "_enadev", dev);
                        fprintf(stderr, "[audio] UCM: _enadev=%s %s\n",
                                dev, rr == 0 ? "ok" : "FAIL");
                        if (rr == 0 && (strstr(dev, "Speaker") ||
                                        strstr(dev, "Speakers")))
                            enabled_speaker = 1;
                    }
                    snd_use_case_free_list(devlist, ndev);
                } else {
                    /* Fallback: enable Speaker variants only. */
                    const char *names[] = {"Speaker", "Speakers", NULL};
                    for (int i = 0; names[i]; i++) {
                        if (snd_use_case_set(uc, "_enadev", names[i]) == 0) {
                            fprintf(stderr, "[audio] UCM: _enadev=%s ok\n",
                                    names[i]);
                            enabled_speaker = 1;
                        }
                    }
                }
                if (!enabled_speaker)
                    fprintf(stderr, "[audio] UCM: WARNING no Speaker device enabled\n");
                snd_use_case_mgr_close(uc);
            } else {
                fprintf(stderr, "[audio] UCM: no config matched card '%s' — manual mixer fallback\n",
                        card_id);
            }
        }

        /* Defensive audio diagnostic — dump the full ASoC DAPM graph and
         * every kcontrol's current value so post-mortem log analysis can
         * tell whether a PGA is sitting at -inf, a DAPM widget is stuck
         * OFF, or a DAI isn't active. These files are debugfs-backed so
         * require CONFIG_DEBUG_FS=y and debugfs mounted at
         * /sys/kernel/debug (init already does the mount). Non-fatal if
         * absent. */
        {
            const char *dbg_dir = "/sys/kernel/debug/asoc/card0";
            if (access(dbg_dir, R_OK) == 0) {
                fprintf(stderr, "[audio-diag] ASoC debugfs dump — %s\n", dbg_dir);
                /* dapm/ subdir has one file per widget with its power state,
                 * input/output connections, and active stream info. */
                DIR *dapm = opendir("/sys/kernel/debug/asoc/card0/dapm");
                if (dapm) {
                    struct dirent *de;
                    while ((de = readdir(dapm))) {
                        if (de->d_name[0] == '.') continue;
                        char widget_path[256];
                        snprintf(widget_path, sizeof(widget_path),
                                 "/sys/kernel/debug/asoc/card0/dapm/%s",
                                 de->d_name);
                        FILE *wf = fopen(widget_path, "r");
                        if (!wf) continue;
                        /* Each widget file's first line is the state:
                         * "WidgetName: On in 0 out 0 stream ..." */
                        char wline[256];
                        if (fgets(wline, sizeof(wline), wf)) {
                            char *nl = strchr(wline, '\n');
                            if (nl) *nl = 0;
                            fprintf(stderr, "[audio-diag] dapm: %s\n", wline);
                        }
                        fclose(wf);
                    }
                    closedir(dapm);
                }
                /* /sys/kernel/debug/gpio dump shows MAX98360A SD_MODE +
                 * RT5682 IRQ GPIO current state so we can tell if the
                 * amp was powered at snapshot time. */
                FILE *gf = fopen("/sys/kernel/debug/gpio", "r");
                if (gf) {
                    char gline[256];
                    int lines = 0;
                    while (lines < 30 && fgets(gline, sizeof(gline), gf)) {
                        char *nl = strchr(gline, '\n');
                        if (nl) *nl = 0;
                        if (strstr(gline, "sdmode") || strstr(gline, "RT58") ||
                            strstr(gline, "gpiochip")) {
                            fprintf(stderr, "[audio-diag] gpio: %s\n", gline);
                            lines++;
                        }
                    }
                    fclose(gf);
                }
            } else {
                fprintf(stderr, "[audio-diag] debugfs not mounted — no ASoC state available\n");
            }
        }
    }

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

            // Log all mixer elements — with pre-set values so a silent audio
            // log can be diagnosed without flashing again. If a playback-
            // switch element is already off before our unmute, or a volume
            // element reports a suspicious range (e.g. min==max at 0), we
            // want to know which one.
            fprintf(stderr, "[audio] Mixer: %s", name);
            if (snd_mixer_selem_has_playback_volume(elem)) {
                long vmin = 0, vmax = 0, vcur = 0;
                snd_mixer_selem_get_playback_volume_range(elem, &vmin, &vmax);
                snd_mixer_selem_get_playback_volume(elem, SND_MIXER_SCHN_FRONT_LEFT, &vcur);
                fprintf(stderr, " [vol %ld..%ld now=%ld]", vmin, vmax, vcur);
                long dbmin = 0, dbmax = 0, dbcur = 0;
                if (snd_mixer_selem_get_playback_dB_range(elem, &dbmin, &dbmax) == 0 &&
                    snd_mixer_selem_get_playback_dB(elem, SND_MIXER_SCHN_FRONT_LEFT, &dbcur) == 0) {
                    fprintf(stderr, " [dB %.1f..%.1f now=%.1f]",
                            dbmin / 100.0, dbmax / 100.0, dbcur / 100.0);
                }
            }
            if (snd_mixer_selem_has_playback_switch(elem)) {
                int sw = 0;
                snd_mixer_selem_get_playback_switch(elem, SND_MIXER_SCHN_FRONT_LEFT, &sw);
                fprintf(stderr, " [sw now=%s]", sw ? "on" : "OFF");
            }
            if (snd_mixer_selem_has_capture_switch(elem)) fprintf(stderr, " [cap-sw]");
            if (snd_mixer_selem_has_capture_volume(elem)) fprintf(stderr, " [cap-vol]");
            fprintf(stderr, "\n");

            // Unmute every playback switch we find — except the ones
            // the UCM BootSequence explicitly turns off to keep audio
            // routed to the speaker amp on unplugged-headphone state.
            // Flipping "Headphone Jack Switch" on re-enables the HP
            // DAPM path and silences MAX98360A even when nothing is
            // plugged in (see G7/Drawcia debug session).
            if (snd_mixer_selem_has_playback_switch(elem)) {
                int skip = 0;
                const char *jack_gated[] = {
                    "Headphone Jack",        /* rt5682 */
                    "Headphone Jack Switch",
                    "HPOL Playback",
                    "HPOR Playback",
                    "Headset",
                    NULL
                };
                for (int j = 0; jack_gated[j]; j++) {
                    if (strstr(name, jack_gated[j])) { skip = 1; break; }
                }
                if (!skip) {
                    snd_mixer_selem_set_playback_switch_all(elem, 1);
                    fprintf(stderr, "[audio] Unmuted: %s\n", name);
                } else {
                    fprintf(stderr, "[audio] Skip unmute (jack-gated): %s\n", name);
                }
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

    // Read initial system volume. On SOF cards there's no Master mixer
    // and read_system_volume_card returns -1 — default to 150% (2.25×
    // linear) which is loud enough on tiny Chromebook speakers without
    // pushing soft_clip into audible distortion. Volume keys still go
    // up to 400% for very quiet hardware.
    int hw_vol = read_system_volume_card(card_idx);
    audio->system_volume = (hw_vol >= 0) ? hw_vol : 180;
    fprintf(stderr, "[audio] System volume: %d%% (hw=%d)\n",
            audio->system_volume, hw_vol);

    // HDMI audio disabled — opening HDMI PCM streams on the same HDA controller
    // can exhaust controller streams and cause EIO on capture.
    audio->hdmi_pcm = NULL;
    fprintf(stderr, "[audio] HDMI audio: disabled\n");

    // Start audio thread
    audio->running = 1;
    pthread_create(&audio->thread, NULL, audio_thread_fn, audio);

    /* Force PCI runtime-PM to "on" for the sound card now that the
     * driver is loaded and card_idx is known. The init-script attempt
     * may fire before probe — doing it here guarantees the sysfs node
     * exists. Without this the SOF DSP auto-suspends after ~20-40s
     * of silence, which stops the SSP1 BE DAI and drops MAX98360A
     * sdmode to 0 (speaker amp off), and the amp never comes back.
     * Also try the generic PCI "power_save" disable for HDA paths. */
    {
        char pm_path[128];
        snprintf(pm_path, sizeof(pm_path),
                 "/sys/class/sound/card%d/device/power/control", card_idx);
        FILE *pm = fopen(pm_path, "w");
        if (pm) {
            fputs("on", pm);
            fclose(pm);
            fprintf(stderr, "[audio] Disabled runtime-PM: %s\n", pm_path);
        } else {
            fprintf(stderr, "[audio] Could not set runtime-PM: %s\n", pm_path);
        }
        /* Also try the autosuspend delay — set to -1 (never) */
        snprintf(pm_path, sizeof(pm_path),
                 "/sys/class/sound/card%d/device/power/autosuspend_delay_ms",
                 card_idx);
        pm = fopen(pm_path, "w");
        if (pm) {
            fputs("-1", pm);
            fclose(pm);
            fprintf(stderr, "[audio] Set autosuspend_delay=-1: %s\n", pm_path);
        }
    }

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

    if (type == WAVE_NOISE || type == WAVE_WHISTLE || type == WAVE_GUN ||
        type == WAVE_HARP || type == WAVE_PIANO ||
        type == WAVE_GMPIANO || type == WAVE_EPIANO || type == WAVE_PLUCK) {
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
    } else if (type == WAVE_HARP) {
        // Karplus-Strong pluck: seed the delay line with one wavelength
        // of pre-smoothed white noise. The initial noise IS the pluck —
        // attack is instantaneous, and the circulating filter+decay
        // shapes it into a plucked-string tone.
        //   Karplus & Strong (1983), Computer Music Journal 7(2), 43-55.
        memset(v->whistle_bore_buf, 0, sizeof(v->whistle_bore_buf));
        v->harp_lp1 = 0.0;
        double sr = (double)(audio->actual_rate ? audio->actual_rate : AUDIO_SAMPLE_RATE);
        double string_delay = sr / freq;
        const int STRING_N = 2048;
        if (string_delay > (double)(STRING_N - 2)) string_delay = (double)(STRING_N - 2);
        if (string_delay < 2.0) string_delay = 2.0;
        int n = (int)string_delay;
        // Pre-smooth the excitation so the initial transient is softer
        // (Jaffe-Smith "pick direction" / brightness control, simplified).
        double last = 0.0;
        for (int i = 0; i < n; i++) {
            double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
            double filt = 0.5 * (white + last);
            last = white;
            v->whistle_bore_buf[i] = (float)filt;
        }
        // Next write lands past the pluck; first read pulls buf[0].
        v->whistle_bore_w = n;
    } else if (type == WAVE_PIANO) {
        // Sample-bank piano. Find the nearest anchor in piano_bank by
        // MIDI distance, set step = 2^((target_midi - anchor_midi)/12)
        // for pitch shift, position = 0 to start playback from the
        // hammer attack. If no bank loaded (samples missing on the
        // initramfs), the voice produces silence — non-fatal.
        double f0 = freq < 20.0 ? 20.0 : freq;
        // Convert frequency to MIDI: midi = 69 + 12*log2(f / 440).
        double target_midi_d = 69.0 + 12.0 * log2(f0 / 440.0);
        int    target_midi   = (int)(target_midi_d + 0.5);

        const PianoSample *anchor = pick_piano_anchor(target_midi);
        if (anchor && anchor->data && anchor->len > 0) {
            v->piano_sample_data = anchor->data;
            v->piano_sample_len  = anchor->len;
            v->piano_sample_pos  = 0.0;
            // step = 2^((target - anchor)/12). Positive offset → faster
            // playback → higher pitch. Sample is at AUDIO_SAMPLE_RATE
            // already (decimated at build time), so no sample-rate
            // correction is needed beyond the pitch ratio.
            double semis = target_midi_d - (double)anchor->midi;
            v->piano_sample_step = pow(2.0, semis / 12.0);
            // Salamander samples sit several dB below the ±1 oscillators
            // (recorded with headroom across velocity layers), so a bare
            // pass-through reads quieter than sine/square at the same
            // notepat `volume`. 3.0× brings perceived loudness in line
            // with the basic waves; soft_clip absorbs any peak excess.
            v->piano_sample_amp  = 3.0;
            ac_log("[piano] f0=%.1fHz midi=%d → anchor midi=%d step=%.4f len=%d\n",
                   f0, target_midi, anchor->midi,
                   v->piano_sample_step, anchor->len);
        } else {
            v->piano_sample_data = NULL;
            v->piano_sample_len  = 0;
            v->piano_sample_pos  = 0.0;
            v->piano_sample_step = 1.0;
            v->piano_sample_amp  = 0.0;
            ac_log("[piano] no bank loaded — voice silent (target midi=%d)\n",
                   target_midi);
        }
    }

    pthread_mutex_unlock(&audio->lock);
    return v->id;
}

// GM synthesis voice (docs/gm-synthesis/01). Mirrors audio_synth_gun: do the
// base voice setup (slot alloc, envelope fields, per-trigger noise_seed) via
// audio_synth with a GM placeholder type so the seed is set, then run
// gm_voice_init() to select the real engine + fill its state + apply note-on
// stochasticism. Programs outside 0..7 return 0 so the JS caller can fall
// back to the normal `type`-based audio_synth path. The base init passes
// WAVE_GMPIANO purely to trigger seeding — gm_voice_init overwrites v->type.
uint64_t audio_synth_gm(ACAudio *audio, int program, double freq,
                        double duration, double volume, double attack,
                        double decay, double pan) {
    if (!audio) return 0;
    if (program < 0 || program >= GM_PIANO_PROGRAM_COUNT) return 0;
    uint64_t id = audio_synth(audio, WAVE_GMPIANO, freq, duration, volume,
                              attack, decay, pan);
    if (!id) return 0;

    pthread_mutex_lock(&audio->lock);
    ACVoice *v = NULL;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id) { v = &audio->voices[i]; break; }
    }
    if (v) {
        double sr = (double)(audio->actual_rate ? audio->actual_rate : AUDIO_SAMPLE_RATE);
        if (gm_voice_init(v, program, freq, sr, &v->noise_seed) < 0) {
            // Shouldn't happen (range checked above) — leave as a harmless
            // GMPIANO voice rather than a half-init state.
            v->type = WAVE_GMPIANO;
        }
    }
    pthread_mutex_unlock(&audio->lock);
    return id;
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

// Apply a single per-shot param override to a freshly-initialized gun
// voice. Called by the JS bindings between audio_synth_gun() and the
// audio thread's first read of the voice — lets the inspector's
// drag-to-edit cards push live tuning values into the next shot
// without rebuilding gun_presets[]. Unknown keys are silently ignored.
//
// Layer-state fields (envelopes, biquad coefficients, the Friedlander
// pulse position) are NOT exposed; we only change the constants the
// preset would have set in init.
void audio_gun_voice_set_param(ACAudio *audio, uint64_t id,
                               const char *key, double value) {
    if (!audio || !key) return;
    pthread_mutex_lock(&audio->lock);
    ACVoice *v = NULL;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id && audio->voices[i].type == WAVE_GUN) {
            v = &audio->voices[i];
            break;
        }
    }
    if (!v) { pthread_mutex_unlock(&audio->lock); return; }

    double sr = (double)(audio->actual_rate ? audio->actual_rate
                                            : AUDIO_SAMPLE_RATE);
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
            // Both freq and Q feed the same biquad, recompute together.
            // For partial updates we just recompute with the latest value
            // and keep the other from existing coefs (lossy but adequate).
            // Approximate Q from a2 = r² → r = √a2 → tau = -π·f/(Q·sr·ln r).
            double a2 = v->gun_body_a2[0];
            double r = a2 > 0 ? sqrt(a2) : 0.95;
            double cur_w = acos(v->gun_body_a1[0] / (2.0 * r));
            double cur_f = cur_w * sr / (2.0 * M_PI);
            double cur_q = -M_PI * cur_f / (sr * log(r > 0.0001 ? r : 0.0001));
            double f = (strcmp(key, "crack_fc") == 0) ? value : cur_f;
            double q = (strcmp(key, "crack_q") == 0) ? value : cur_q;
            compute_resonator(f, q, sr, &v->gun_body_a1[0],
                              &v->gun_body_a2[0], &v->gun_crack_b0);
        }
        else if (strcmp(key, "boom_amp") == 0) v->gun_body_amp[1] = value;
        else if (strcmp(key, "boom_freq_start") == 0) {
            v->gun_boom_freq_start = value;
            v->gun_boom_freq = value;
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
            compute_resonator(f, q, sr, &v->gun_body_a1[1],
                              &v->gun_body_a2[1], &v->gun_tail_b0);
        }
    } else {
        // Physical model overrides.
        if      (strcmp(key, "pressure") == 0) v->gun_pressure = value;
        else if (strcmp(key, "env_rate") == 0) {
            v->gun_phys_t_plus = (3.0 / (value > 100 ? value : 100.0)) * sr;
            if (v->gun_phys_t_plus < 32.0) v->gun_phys_t_plus = 32.0;
            if (v->gun_phys_t_plus > 4096.0) v->gun_phys_t_plus = 4096.0;
        }
        else if (strcmp(key, "bore_length_s") == 0) {
            v->gun_bore_delay = value * sr;
            if (v->gun_bore_delay < 4.0) v->gun_bore_delay = 4.0;
            if (v->gun_bore_delay > 2040.0) v->gun_bore_delay = 2040.0;
        }
        else if (strcmp(key, "bore_loss") == 0) v->gun_bore_loss = value;
        else if (strcmp(key, "breech_reflect") == 0) v->gun_breech_reflect = value;
        else if (strcmp(key, "noise_gain") == 0) v->gun_noise_gain = value;
        else if (strcmp(key, "radiation") == 0) v->gun_radiation_a = value;
        else if (strncmp(key, "body_freq", 9) == 0 ||
                 strncmp(key, "body_q", 6) == 0) {
            int idx = key[strlen(key) - 1] - '0';
            if (idx < 0 || idx > 2) { pthread_mutex_unlock(&audio->lock); return; }
            // Recompute the resonator with one swapped param, the other inferred.
            double a2 = v->gun_body_a2[idx];
            double r = a2 > 0 ? sqrt(a2) : 0.95;
            double cur_w = acos(v->gun_body_a1[idx] / (2.0 * r));
            double cur_f = cur_w * sr / (2.0 * M_PI);
            double cur_q = -M_PI * cur_f / (sr * log(r > 0.0001 ? r : 0.0001));
            double f = (strncmp(key, "body_freq", 9) == 0) ? value : cur_f;
            double q = (strncmp(key, "body_q", 6) == 0) ? value : cur_q;
            double b0_unused;
            compute_resonator(f, q, sr, &v->gun_body_a1[idx],
                              &v->gun_body_a2[idx], &b0_unused);
        }
        else if (strncmp(key, "body_amp", 8) == 0) {
            int idx = key[strlen(key) - 1] - '0';
            if (idx >= 0 && idx <= 2) v->gun_body_amp[idx] = value;
        }
    }
    pthread_mutex_unlock(&audio->lock);
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

// User-exposed master gain. Range 0..2 (200%) — above that you're almost
// certainly just hitting soft_clip and colouring the signal, so clamp
// before that to avoid giving false "louder" feedback in the UI slider.
void audio_set_master_volume(ACAudio *audio, float value) {
    if (!audio) return;
    if (value < 0.0f) value = 0.0f;
    if (value > 2.0f) value = 2.0f;
    audio->target_master_volume = value;
}

// Drive amount 0..1 dry/wet blend. 0 = clean bypass, 1 = fully driven
// (pre-gain × 6 into tanh, attenuated back). Smoothed per-sample so
// sliding the fader doesn't audibly zipper.
void audio_set_drive_mix(ACAudio *audio, float value) {
    if (!audio) return;
    if (value < 0.0f) value = 0.0f;
    if (value > 1.0f) value = 1.0f;
    audio->target_drive_mix = value;
}

// Global GM-synthesis organic amount (docs/gm-synthesis/00-stochasticism.md).
// Scales every parametric note-on jitter lever; 0 restores bit-identical
// synthesis. `audio` is accepted for API symmetry but the value lives in a
// file-static so the note-on helpers stay dependency-free in the inner code.
void audio_set_organic(double amt) {
    g_organic_amount = clampd(amt, 0.0, 1.0);
}

// Wobble / flange dry/wet 0..1. LFO-modulated short delay blended with
// the dry signal. Same exponential smoother as the other mix params so
// a fader sweep doesn't click. The LFO rate itself is fixed at 0.4 Hz
// and doesn't need smoothing.
void audio_set_wobble_mix(ACAudio *audio, float value) {
    if (!audio) return;
    if (value < 0.0f) value = 0.0f;
    if (value > 1.0f) value = 1.0f;
    audio->target_wobble_mix = value;
}

// Pause/resume writes to output_history_buf. Called from notepat while
// the spacebar is held for reverse-replay: with writes paused the reverse
// playback echo (re-captured from the speaker mix) can't double-layer
// over the original wave in the visualizer AND any overdub played during
// the hold is purely monitored — not written into the capture ring.
// Release un-pauses; writes pick back up exactly where they left off so
// the buffer boundary is invisible to downstream consumers.
void audio_set_output_history_paused(ACAudio *audio, int paused) {
    if (!audio) return;
    audio->output_history_paused = paused ? 1 : 0;
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

// Unmute all playback switches in the mixer — but skip jack-gated
// ones so we don't re-enable the headphone DAPM path (see audio_init
// above for the MAX98360A silencing story).
static void unmute_all_switches(snd_mixer_t *mixer) {
    snd_mixer_elem_t *elem;
    const char *jack_gated[] = {
        "Headphone Jack", "Headphone Jack Switch",
        "HPOL Playback", "HPOR Playback", "Headset", NULL
    };
    for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
        if (!snd_mixer_selem_is_active(elem)) continue;
        if (!snd_mixer_selem_has_playback_switch(elem)) continue;
        const char *name = snd_mixer_selem_get_name(elem);
        int skip = 0;
        for (int j = 0; jack_gated[j]; j++) {
            if (name && strstr(name, jack_gated[j])) { skip = 1; break; }
        }
        if (!skip)
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
    /* RT5682 exposes "DAC1" for digital volume; SOF cards expose
     * "PGA*.0 * Master" pipeline PGAs. HDA laptops expose Master/PCM.
     * Try everything — first match wins but we run through the whole
     * list so volume keys work regardless of hardware. */
    const char *try_names[] = {"Master", "Speaker", "Headphone", "PCM",
                                "DAC1", "DAC2",
                                "PGA1.0 1 Master", "PGA2.0 2 Master",
                                "PGA5.0 5 Master", "PGA6.0 6 Master",
                                "PGA7.0 7 Master", NULL};
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

    // Update cached system volume. On SOF cards without a "Master" mixer,
    // read_system_volume_card returns -1. In that case, use software-only
    // volume: start at 100 and step ±5 with volume keys.
    if (muted) {
        audio->system_volume = 0;
    } else {
        int hw_vol = read_system_volume_card(audio->card_index);
        if (hw_vol >= 0) {
            audio->system_volume = hw_vol;
        } else {
            // No Master mixer — software gain mode (0..400%).
            // 150% is the default boot volume on SOF cards; allow
            // keys to step up to 400 (16× linear) for quiet speakers.
            int sv = audio->system_volume;
            if (sv < 0) sv = 150;
            int step = 10;
            if (delta > 0) sv = (sv + step > 400) ? 400 : sv + step;
            else if (delta < 0) sv = (sv - step < 0) ? 0 : sv - step;
            audio->system_volume = sv;
            ac_log("[audio] Software volume: %d%%\n", sv);
        }
    }
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
        // Generate waveform peaks for visualization. Skip for live streams
        // (radio): the peak pass reads the source to EOF, which never comes
        // on an endless Icecast feed and would hang the caller.
        if (!dk->decoder->is_stream)
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
    if (audio->headphone_pcm) snd_pcm_close((snd_pcm_t *)audio->headphone_pcm);
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
