// frothyscreama.c — the screamo engine. The composition lives in
// bin/render-frothyscreama.mjs; run with --bake it writes every event it
// played, every random choice already made, and this file plays them again:
// the same wall of guitars, the same kit, the same room, the same tape.
//
// What is here is the sound, not the song:
//
//   place     a strummed guitar take (pop/guitar/c/strum), clean or through
//             the wall — two more tanh stages and a cabinet
//   pick      the twinkle: one Karplus-Strong string per note
//   bass      a Growlybass note, repitched and driven
//   drum      one microphone of one hit of the Virtuosity kit
//   feedback  the sine that climbs out of the bridge
//   modem     the dial-up handshake under the arpeggiated screams
//   vox       one layer of jeffrey, shaped and (for a scream) overdriven
//
// then the dotted-eighth ping-pong, a short room, the sum with the wall
// leaning back under the voice, and the plastic tape (lib/c/ac_material.h).
//
// Build:  ./build.sh
// Run:    ./frothyscreama <score.txt> --raw out.f32 [--vox vox.f32]
//         f32le stereo 48k, the premaster — run-c.mjs masters it.
//
// Score (text, one item per line, written by the renderer's --bake):
//   sr / frames / bpm / material <name|none> / gain <bus> <x>   — globals
//   smp <id> <path.f32>                                         — a decoded sample
//   place <gtr|clean> t gain pan drive <take.wav>
//   pick t midi gain pan
//   bass <smp> i0 n rate drive gain
//   drum <smp> i0 amp rate maxSec wet
//   feedback i0 n
//   modem <smp> i0 n offset rate
//   vox i0 gain pan scream shaped <take.wav|take.f32>

#define _POSIX_C_SOURCE 200809L
#include "../../lib/c/ac_bake.h"
#include "../../lib/c/ac_material.h"

static double SR = 48000, BPM = 180;
static long N;
static float *gtr, *clean, *drums, *vox, *rev;   // stereo, interleaved
static float *bass, *dly;                        // mono
static AcNoise pickNoise = { 0xf207f207 };

// ── guitars ─────────────────────────────────────────────────────────────
// The wall: the engine's amp, then two more tanh stages and a cabinet.
static void wall(float *ch, long n, double drive) {
    double hp = 0, lp1 = 0, lp2 = 0;
    for (long i = 0; i < n; i++) {
        hp += 0.012 * (ch[i] - hp);
        double x = tanh((ch[i] - hp) * drive);
        x = tanh(x * 2.2 + 0.08) - 0.08;
        lp1 += 0.48 * (x - lp1); lp2 += 0.48 * (lp1 - lp2);
        ch[i] = (float)lp2;
    }
}

static void place(float *bus, const char *path, double t, double gain, double pan, double drive) {
    float *L, *R;
    long n;
    ac_read_wav(path, &L, &R, &n);
    if (drive > 0) { wall(L, n, drive); wall(R, n, drive); }
    const long i0 = ac_round(t * SR);
    const double gl = gain * ac_pan_l(pan), gr = gain * ac_pan_r(pan);
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double m = 0.5 * ((double)L[k] + R[k]);
        const long j = (i0 + k) * 2;
        ac_add(bus, j, m * gl); ac_add(bus, j + 1, m * gr);
        ac_add(rev, j, m * gl * 0.12); ac_add(rev, j + 1, m * gr * 0.12);
    }
    free(L); free(R);
}

// Twinkle: one Karplus-Strong string per note, picked clean.
static void pick(double t, double midi, double gain, double pan) {
    const double f = ac_mtof(midi);
    const long len = ac_round(SR / f);
    float *line = ac_alloc((size_t)len, sizeof(float));
    double lp = 0;
    for (long i = 0; i < len; i++) { lp += 0.45 * (ac_noise(&pickNoise) - lp); line[i] = (float)lp; }
    const long i0 = ac_round(t * SR), n = (long)floor(1.6 * SR);
    const double gl = gain * ac_pan_l(pan), gr = gain * ac_pan_r(pan);
    long idx = 0;
    double prev = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double y = line[idx], v = 0.5 * (y + prev) * 0.9975;
        prev = y; line[idx] = (float)v; if (++idx == len) idx = 0;
        const long j = (i0 + k) * 2;
        ac_add(clean, j, y * gl); ac_add(clean, j + 1, y * gr);
        ac_add(rev, j, y * gl * 0.5); ac_add(rev, j + 1, y * gr * 0.5);
        ac_add(dly, i0 + k, y * gain * 0.35);
    }
    free(line);
}

// ── bass ────────────────────────────────────────────────────────────────
static void bass_note(const AcSmp *s, long i0, long n, double rate, double drive, double gain) {
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double p = k * rate, q = floor(p), fr = p - q;
        const long qi = (long)q;
        if (qi + 1 >= s->n) break;
        const double env = fmin(1, k / 96.0) * fmin(1, (n - k) / (0.06 * SR));
        ac_add(bass, i0 + k, tanh((s->d[qi] * (1 - fr) + s->d[qi + 1] * fr) * 4 * drive) / tanh(drive) * env * gain * 1.15);
    }
}

// ── drums ───────────────────────────────────────────────────────────────
// One microphone of one strike: stereo, replayed at a rate, faded at the end.
static void drum(const AcSmp *s, long i0, double a, double rate, double max, double wet) {
    const double frames = fmin((double)(s->n >> 1), floor(max * SR * rate)), fade = 0.3 * SR;
    const long len = (long)floor(frames / rate);
    for (long k = 0; k < len && i0 + k < N; k++) {
        const long q = rate == 1 ? k : (long)floor(k * rate), j = (i0 + k) * 2;
        const double e = fmin(1, (len - k) / fade);
        const double l = s->d[q * 2] * a * e, r = s->d[q * 2 + 1] * a * e;
        ac_add(drums, j, l); ac_add(drums, j + 1, r);
        ac_add(rev, j, l * wet); ac_add(rev, j + 1, r * wet);
    }
}

// feedback into the second explosion: one sine climbing out of the bridge
static void feedback(long i0, long n) {
    double ph = 0;
    for (long k = 0; k < n; k++) {
        const double u = (double)k / n;
        ph += ac_mtof(78 + 0.3 * sin(AC_TAU * 5 * k / SR)) / SR; if (ph >= 1) ph -= 1;
        const double v = tanh(3 * sin(AC_TAU * ph)) * u * u * 0.09;
        const long j = (i0 + k) * 2;
        if (i0 + k >= N) break;
        ac_add(gtr, j, v); ac_add(gtr, j + 1, v * 0.7);
        ac_add(rev, j, v * 0.5); ac_add(rev, j + 1, v * 0.5);
    }
}

// ── jeffrey ─────────────────────────────────────────────────────────────
static void biquad_peak(float *x, long n, double hz, double db, double q) {
    const double A = pow(10.0, db / 40), w = AC_TAU * hz / SR, al = sin(w) / (2 * q), a0 = 1 + al / A;
    const double b0 = (1 + al * A) / a0, b1 = -2 * cos(w) / a0, b2 = (1 - al * A) / a0, a2 = (1 - al / A) / a0;
    double x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    for (long i = 0; i < n; i++) {
        const double y = b0 * x[i] + b1 * x1 + b2 * x2 - b1 * y1 - a2 * y2;
        x2 = x1; x1 = x[i]; y2 = y1; y1 = y; x[i] = (float)y;
    }
}

// A pop vocal chain per take: rumble out, a little low-mid out, presence in,
// then a 4:1 compressor so every word sits at the front. Peak-normalized.
static void shape_vocal(float *x, long n) {
    double h1 = 0, h2 = 0;
    const double kh = 1 - exp(-AC_TAU * 110 / SR);
    for (long i = 0; i < n; i++) {
        h1 += kh * (x[i] - h1);
        const double a = x[i] - h1;
        h2 += kh * (a - h2);
        x[i] = (float)(a - h2);
    }
    biquad_peak(x, n, 300, -2.5, 1.0); biquad_peak(x, n, 3200, 3.5, 0.9); biquad_peak(x, n, 7500, -2, 1.5);
    double pk = 1e-9;
    for (long i = 0; i < n; i++) if (fabs(x[i]) > pk) pk = fabs(x[i]);
    const double thr = 0.14, atk = exp(-1 / (0.004 * SR)), rel = exp(-1 / (0.09 * SR));
    double env = 0, out = 1e-9;
    for (long i = 0; i < n; i++) {
        const double v = x[i] / pk, a = fabs(v);
        env = a > env ? a + (env - a) * atk : a + (env - a) * rel;
        x[i] = (float)(v * (env > thr ? (thr + (env - thr) / 4) / env : 1));
        if (fabs(x[i]) > out) out = fabs(x[i]);
    }
    for (long i = 0; i < n; i++) x[i] = (float)(x[i] / out);
}

// A real dial-up handshake under the arpeggiated screams, swapping ears.
static void modem(const AcSmp *s, long i0, long n, long off, double rate) {
    for (long k = 0; k < n && i0 + k < N; k++) {
        const long q = (long)floor(off + k * rate), j = (i0 + k) * 2;
        const double src = q >= 0 && q < s->n ? s->d[q] : 0;
        const double v = src * ac_min3(1, k / 2400.0, (n - k) / 9600.0) * 0.07;
        ac_add(vox, j, v * (k % 9600 < 4800 ? 1 : 0.4)); ac_add(vox, j + 1, v * (k % 9600 < 4800 ? 0.4 : 1));
    }
}

static void vox_layer(const char *path, long i0, double gain, double pan, int scream, int shaped) {
    float *src, *other = NULL;
    long n;
    const char *dot = strrchr(path, '.');
    if (dot && !strcmp(dot, ".wav")) ac_read_wav(path, &src, &other, &n);
    else src = ac_read_f32(path, &n);
    if (shaped) shape_vocal(src, n);
    const double gl = gain * ac_pan_l(pan), gr = gain * ac_pan_r(pan), wet = scream ? 0.07 : 0.06;
    double pk = 1e-9, hp = 0;
    for (long k = 0; k < n; k++) if (fabs(src[k]) > pk) pk = fabs(src[k]);
    for (long k = 0; k < n && i0 + k < N; k++) {
        hp += 0.02 * (src[k] - hp);
        double v = (src[k] - hp) / pk;
        if (scream) v = tanh(v * 2.6) * 0.8;          // the scream into the mic pre, hard
        const long j = (i0 + k) * 2;
        ac_add(vox, j, v * gl); ac_add(vox, j + 1, v * gr);
        ac_add(rev, j, v * gl * wet); ac_add(rev, j + 1, v * gr * wet);   // dry and up front; the room is for the band
        ac_add(dly, i0 + k, v * gain * (scream ? 0.03 : 0.04));
    }
    free(src); free(other);
}

// ── post ────────────────────────────────────────────────────────────────
// dotted-eighth ping-pong for the quiet things
static void ping_pong(void) {
    const long D = ac_round(0.75 * (60 / BPM) * SR);
    float *L = ac_alloc((size_t)D, sizeof(float)), *R = ac_alloc((size_t)D, sizeof(float));
    double a = 0, b = 0;
    for (long i = 0; i < N; i++) {
        const long w = i % D;
        const double oL = L[w], oR = R[w];
        a += 0.5 * (oR - a); b += 0.5 * (oL - b);
        L[w] = (float)(dly[i] + 0.38 * a); R[w] = (float)b;
        ac_add(clean, i * 2, oL * 0.7); ac_add(clean, i * 2 + 1, oR * 0.7);
    }
    free(L); free(R);
}

// a short room: eight damped combs and four allpasses per ear, in place
static void room(void) {
    static const int COMB[8] = { 1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617 }, AP[4] = { 556, 441, 341, 225 };
    const double scale = SR / 44100;
    for (int ch = 0; ch < 2; ch++) {
        float *cb[8], *ab[4];
        long cn[8], ci[8] = { 0 }, an[4], ai[4] = { 0 };
        double clp[8] = { 0 };
        for (int c = 0; c < 8; c++) { cn[c] = ac_round((COMB[c] + ch * 23) * scale); cb[c] = ac_alloc((size_t)cn[c], sizeof(float)); }
        for (int a = 0; a < 4; a++) { an[a] = ac_round((AP[a] + ch * 23) * scale); ab[a] = ac_alloc((size_t)an[a], sizeof(float)); }
        for (long i = 0; i < N; i++) {
            const double x = rev[i * 2 + ch];
            double y = 0;
            for (int c = 0; c < 8; c++) {
                const double o = cb[c][ci[c]];
                y += o; clp[c] = o * 0.6 + clp[c] * 0.4;
                cb[c][ci[c]] = (float)(x + clp[c] * 0.84);
                if (++ci[c] == cn[c]) ci[c] = 0;
            }
            for (int a = 0; a < 4; a++) {
                const double o = ab[a][ai[a]];
                ab[a][ai[a]] = (float)(y + o * 0.5); y = o - y;
                if (++ai[a] == an[a]) ai[a] = 0;
            }
            rev[i * 2 + ch] = (float)(y * 0.1);
        }
        for (int c = 0; c < 8; c++) free(cb[c]);
        for (int a = 0; a < 4; a++) free(ab[a]);
    }
}

int main(int argc, char **argv) {
    ac_engine = "frothyscreama";
    const char *scorePath = NULL, *rawPath = NULL, *voxPath = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--raw") && i + 1 < argc) rawPath = argv[++i];
        else if (!strcmp(argv[i], "--vox") && i + 1 < argc) voxPath = argv[++i];
        else scorePath = argv[i];
    }
    if (!scorePath || !rawPath) ac_die("usage: frothyscreama <score.txt> --raw <out.f32> [--vox <vox.f32>]", NULL);

    FILE *f = fopen(scorePath, "r");
    if (!f) ac_die("cannot open", scorePath);
    char material[64] = "tape", line[8192], *tok[16];
    double Ggtr = 0.85, Gclean = 1.0, Gdrums = 1.1, Gbass = 0.85, Gvox = 1.8, Grev = 1.0;
    long events = 0;
    while (fgets(line, sizeof line, f)) {
        const int n = ac_split(line, tok, 16);
        if (!n) continue;
        const char *v = tok[0];
#define ARGS(k) if (n < (k) + 1) ac_die("short line for", v)
        if (!strcmp(v, "sr")) { ARGS(1); SR = ac_num(tok[1]); }
        else if (!strcmp(v, "bpm")) { ARGS(1); BPM = ac_num(tok[1]); }
        else if (!strcmp(v, "material")) { ARGS(1); snprintf(material, sizeof material, "%s", tok[1]); }
        else if (!strcmp(v, "gain")) {
            ARGS(2);
            const double g = ac_num(tok[2]);
            if (!strcmp(tok[1], "gtr")) Ggtr = g; else if (!strcmp(tok[1], "clean")) Gclean = g;
            else if (!strcmp(tok[1], "drums")) Gdrums = g; else if (!strcmp(tok[1], "bass")) Gbass = g;
            else if (!strcmp(tok[1], "vox")) Gvox = g; else if (!strcmp(tok[1], "rev")) Grev = g;
        }
        else if (!strcmp(v, "frames")) {
            ARGS(1);
            N = (long)ac_num(tok[1]);
            gtr = ac_alloc((size_t)N * 2, sizeof(float)); clean = ac_alloc((size_t)N * 2, sizeof(float));
            drums = ac_alloc((size_t)N * 2, sizeof(float)); vox = ac_alloc((size_t)N * 2, sizeof(float));
            rev = ac_alloc((size_t)N * 2, sizeof(float));
            bass = ac_alloc((size_t)N, sizeof(float)); dly = ac_alloc((size_t)N, sizeof(float));
        }
        else if (!strcmp(v, "smp")) { ARGS(2); ac_smp_load(atoi(tok[1]), tok[2]); }
        else {
            if (!N) ac_die("an event came before `frames`:", v);
            events++;
            if (!strcmp(v, "place")) { ARGS(6); place(!strcmp(tok[1], "gtr") ? gtr : clean, tok[6], ac_num(tok[2]), ac_num(tok[3]), ac_num(tok[4]), ac_num(tok[5])); }
            else if (!strcmp(v, "pick")) { ARGS(4); pick(ac_num(tok[1]), ac_num(tok[2]), ac_num(tok[3]), ac_num(tok[4])); }
            else if (!strcmp(v, "bass")) { ARGS(6); bass_note(ac_smp_get(tok[1]), (long)ac_num(tok[2]), (long)ac_num(tok[3]), ac_num(tok[4]), ac_num(tok[5]), ac_num(tok[6])); }
            else if (!strcmp(v, "drum")) { ARGS(6); drum(ac_smp_get(tok[1]), (long)ac_num(tok[2]), ac_num(tok[3]), ac_num(tok[4]), ac_num(tok[5]), ac_num(tok[6])); }
            else if (!strcmp(v, "feedback")) { ARGS(2); feedback((long)ac_num(tok[1]), (long)ac_num(tok[2])); }
            else if (!strcmp(v, "modem")) { ARGS(5); modem(ac_smp_get(tok[1]), (long)ac_num(tok[2]), (long)ac_num(tok[3]), (long)ac_num(tok[4]), ac_num(tok[5])); }
            else if (!strcmp(v, "vox")) { ARGS(6); vox_layer(tok[6], (long)ac_num(tok[1]), ac_num(tok[2]), ac_num(tok[3]), atoi(tok[4]), atoi(tok[5])); }
            else ac_die("unknown event", v);
        }
#undef ARGS
    }
    fclose(f);
    if (!N) ac_die("the score has no `frames` line", NULL);

    ping_pong();
    room();

    // the sum — the wall leans back a little whenever he is screaming
    float *mix = ac_alloc((size_t)N * 2, sizeof(float));
    double peak = 0, env = 0;
    long bad = 0;
    for (long i = 0; i < N; i++) for (int ch = 0; ch < 2; ch++) {
        const long j = i * 2 + ch;
        if (ch == 0) { const double a = fabs(vox[j]) + fabs(vox[j + 1]); env += (a > env ? 0.002 : 0.00012) * (a - env); }
        const double lean = 1 - 0.32 * fmin(1, env / 0.25);
        const double v = gtr[j] * Ggtr * lean + clean[j] * Gclean + tanh(drums[j] * 1.5) * 0.75 * Gdrums
                       + bass[i] * Gbass + vox[j] * Gvox + rev[j] * Grev;
        if (!isfinite(v)) bad++;
        mix[j] = (float)v;
        if (fabs(v) > peak) peak = fabs(v);
    }
    if (bad) ac_die("non-finite samples in the mix", NULL);

    if (strcmp(material, "none")) {
        const double pre = 0.6 / peak;
        for (long j = 0; j < N * 2; j++) mix[j] = (float)(mix[j] * pre);
        if (ac_material_apply(mix, N, SR, material, 1.0)) ac_die("unknown material", material);
        peak = 0;
        for (long j = 0; j < N * 2; j++) if (fabs(mix[j]) > peak) peak = fabs(mix[j]);
    }
    const double g = 0.9 / peak;
    for (long j = 0; j < N * 2; j++) mix[j] = (float)(mix[j] * g);

    ac_write_f32(rawPath, mix, N * 2);
    if (voxPath) ac_write_f32(voxPath, vox, N * 2);
    fprintf(stderr, "frothyscreama: %ld events · %.1f s · material %s\n", events, N / SR, material);
    return 0;
}
