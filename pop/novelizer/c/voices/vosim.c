// vosim.c — VOSIM (VOice SIMulation) voice for pop/novelizer.
//
// Technique: Werner Kaegi & Stan Tempelaars, "VOSIM — A New Sound
// Synthesis System", Journal of the Audio Engineering Society 26(6),
// 1978 (Institute of Sonology, Utrecht). Each fundamental period holds
// N sin^2 pulses of width T, each pulse's amplitude b times the previous
// (b < 1), followed by a silent gap M so that N*T + M = 1/f0. The pulse
// width places a formant at F = 1/T; N and b shape the formant's
// bandwidth and spectral rolloff; the gap M sets the fundamental. All
// parameters are latched once per fundamental period, exactly as the
// original hardware oscillator updated its registers, so the waveform
// stays click-free (every pulse starts and ends at zero).
//
// Parameter map (per note):
//   pitch      -> fundamental f0; period = 1/f0, gap M = period - N*T
//   velocity   -> N (2..8 pulses) and b (0.50..0.92): brighter, wider
//                 formant band at high velocity, dull thin buzz at low
//   note time  -> formant F glides through a 6-vowel table
//                 (a e i o u ae); short notes make one diphthong glide,
//                 long notes chew through vowel space at ~0.55 vowels/s,
//                 and b itself breathes slowly (0.09 Hz) on held tones
//   sustain    -> delayed 5.1 Hz vibrato, ~6 cents, fading in at 0.25 s
//
// Novelty vs. the AC stable: nothing else in the stable is a pulse-train
// formant synth. The sine/supersaw/hoover family has no formant at all,
// skrill's talking bass is FM + swept filters (subtractive), zitar is a
// plucked waveguide, nullnoise is carved noise. VOSIM's unipolar sin^2
// pulse packets give a buzzy robot-vowel timbre — a harmonic comb with
// one wandering resonance — that none of those techniques produce.
//
// Expected spectral signature (verified in analysis): clean harmonic
// staircase at f0 with a single broad formant band gliding within each
// note; drone shows continuous centroid motion as it chews vowels.

#include "../novelizer.h"

#define VOWELS 6
/* first-formant targets, ordered for pleasant chewing: a e i o u ae */
static const double vowel_f[VOWELS] = { 720.0, 470.0, 290.0, 520.0, 360.0, 640.0 };

static double smoothstep(double x) { return x * x * (3.0 - 2.0 * x); }

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    const double f0base = n->freq;
    const double vel = n->vel;
    const int s0 = (int)(n->start * NV_SR);
    const double rel = 0.06; /* release seconds after the held duration */
    const int len = (int)((n->dur + rel) * NV_SR);

    /* velocity -> pulse-train shape (formant bandwidth / brightness) */
    int ndes = 2 + (int)floor(vel * 6.0 + 0.5);
    if (ndes < 1) ndes = 1;
    if (ndes > 8) ndes = 8;
    const double bbase = 0.50 + 0.42 * vel;

    /* vowel gesture: one diphthong on short notes, slow chewing on long */
    const double vrate = (n->dur > 1.2) ? 0.55 : 1.0 / (n->dur + 0.05);
    const double vstart = (double)(j % VOWELS);

    /* per-period latched oscillator state */
    double p = 1.0;            /* normalized period phase; 1.0 forces latch */
    double inc = f0base / NV_SR;
    double period = 1.0 / f0base;
    double T = 1.0 / 500.0;    /* pulse width (s) = 1 / formant Hz */
    int N = ndes;              /* pulses this period */
    double amps[9] = { 0 };    /* b^k * gnorm, precomputed at latch */
    double x1 = 0.0, y1 = 0.0; /* DC blocker (VOSIM pulses are unipolar) */

    for (int i = 0; i < len && s0 + i < nframes; i++) {
      const double t = (double)i / NV_SR;

      if (p >= 1.0) { /* new fundamental period: latch all parameters */
        p -= 1.0;
        /* delayed vibrato on sustained notes */
        double f0 = f0base;
        if (n->dur > 0.5 && t > 0.25) {
          const double dep = fmin(1.0, (t - 0.25) / 0.4) * 0.0035;
          f0 *= 1.0 + dep * sin(NV_TAU * 5.1 * t);
        }
        inc = f0 / NV_SR;
        period = 1.0 / f0;
        /* vowel position -> formant frequency F */
        const double v = vstart + vrate * t;
        const int i0 = (int)floor(v) % VOWELS;
        const int i1 = (i0 + 1) % VOWELS;
        const double fr = smoothstep(v - floor(v));
        double F = vowel_f[i0] + (vowel_f[i1] - vowel_f[i0]) * fr;
        /* slow breathing of the decay ratio on held tones */
        double b = bbase * (0.88 + 0.12 * sin(NV_TAU * 0.09 * t + (double)j));
        if (b > 0.97) b = 0.97;
        /* fit N pulses plus >= 8% silent gap into the period */
        if (F < f0 / 0.92) F = f0 / 0.92;
        T = 1.0 / F;
        N = ndes;
        const int nmax = (int)floor(0.92 * F / f0);
        if (N > nmax) N = nmax > 1 ? nmax : 1;
        /* equal-power normalize the pulse group so N/b set color, not level */
        double e = 0.0, a = 1.0;
        for (int k = 0; k < N; k++) { e += a * a; a *= b; }
        const double gnorm = 1.0 / sqrt(e > 1e-9 ? e : 1e-9);
        a = gnorm;
        for (int k = 0; k < N; k++) { amps[k] = a; a *= b; }
      }

      /* VOSIM core: k-th sin^2 pulse or the silent gap */
      const double tin = p * period; /* seconds into the current period */
      double x = 0.0;
      const double kf = tin / T;
      const int k = (int)kf;
      if (k < N) {
        const double s = sin(M_PI * (kf - (double)k));
        x = amps[k] * s * s;
      }
      /* DC blocker: pulses are unipolar, remove the offset */
      const double y = x - x1 + 0.995 * y1;
      x1 = x;
      y1 = y;

      /* amplitude envelope: 6 ms attack, gentle sag, 60 ms release */
      double env;
      if (t < 0.006) env = t / 0.006;
      else if (t < n->dur) env = exp(-0.35 * (t - 0.006) / n->dur);
      else {
        const double e0 = exp(-0.35 * (n->dur - 0.006) / n->dur);
        env = e0 * (1.0 - (t - n->dur) / rel);
        if (env < 0.0) env = 0.0;
      }

      out[s0 + i] += (float)(y * env * (0.25 + 0.75 * vel) * 0.5);
      p += inc;
    }
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "vosim", render); }
